/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mabi=sysv -march=diamondrapids -mapxf -fdump-rtl-ira" } */

/* All pseudos living through the calls are referenced in the loop, so
   a region cannot give them call-saved registers.  */

extern double parse_field (const char *line, int i);
extern void   refill (char *line);

void
load_coords (int natom, double *x, double *v, double *w, char *line)
{
  int n = 3 * natom;
  for (int i = 0; i < n; i++)
    {
      double a = parse_field (line, i);
      x[i] = a;
      v[i] = a * 2.0;
      w[i] = a + x[i > 0 ? i - 1 : 0];
      if (i % 6 == 5 && i < n - 1)
	refill (line);
    }
}

/* { dg-final { scan-rtl-dump {Loop 2 flatten-cost: excess not covered} "ira" } } */
/* { dg-final { scan-rtl-dump-not {flatten-cost:[^\n]*keep} "ira" } } */
