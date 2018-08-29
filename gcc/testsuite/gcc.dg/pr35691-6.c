/* PR middle-end/35691 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noinline, noclone)) int
foo (int *p, unsigned long long *q)
{
  int p0 = p[0], p10 = p[10], p12 = p[12], p32 = p[32], p77 = p[77], p85 = p[85], p86 = p[86], p97 = p[97], p98 = p[98];
  unsigned long long q0 = q[0], q10 = q[10], q12 = q[12], q32 = q[32], q77 = q[77], q85 = q[85], q86 = q[86], q97 = q[97], q98 = q[98];
  return p0 == 0 && q0 == -1 && p10 == 0 && q10 == -1 && p12 == 0 && q12 == -1
	 && p32 == 0 && q32 == -1 && p77 == 0 && q77 == -1 && p85 == 0 && q85 == -1
	 && p86 == 0 && q86 == -1 && p97 == 0 && q97 == -1 && p98 == 0 && q98 == -1;
}

__attribute__((noinline, noclone)) int
bar (int *p, unsigned long long *q)
{
  int p0 = p[0], p10 = p[10], p12 = p[12], p32 = p[32], p77 = p[77], p85 = p[85], p86 = p[86], p97 = p[97], p98 = p[98];
  unsigned long long q0 = q[0], q10 = q[10], q12 = q[12], q32 = q[32], q77 = q[77], q85 = q[85], q86 = q[86], q97 = q[97], q98 = q[98];
  return p0 != 0 | q0 != -1 | p10 != 0 | q10 != -1 | p12 != 0 | q12 != -1
	 | p32 != 0 | q32 != -1 | p77 != 0 | q77 != -1 | p85 != 0 | q85 != -1
	 | p86 != 0 | q86 != -1 | p97 != 0 | q97 != -1 | p98 != 0 | q98 != -1;
}

int p[100];
unsigned long long q[100];

int
main ()
{
  int i;
  for (i = 0; i < 100; i++)
    {
      p[i] = 0;
      q[i] = -1;
    }
  asm volatile ("" : : "g" (p), "g" (q) : "memory");
  if (foo (p, q) != 1 || bar (p, q) != 0)
    __builtin_abort ();
  for (i = 0; i < 100; i++)
    {
      int f1, b1, f2, b2;
      p[i] = 1;
      f1 = foo (p, q);
      b1 = bar (p, q);
      p[i] = 0;
      q[i] = 0;
      f2 = foo (p, q);
      b2 = bar (p, q);
      q[i] = -1;
      switch (i)
	{
	case 0:
	case 10:
	case 12:
	case 32:
	case 77:
	case 85:
	case 86:
	case 97:
	case 98:
	  if (f1 != 0 || b1 != 1 || f2 != 0 || b2 != 1)
	    __builtin_abort ();
	  break;
	default:
	  if (f1 != 1 || b1 != 0 || f2 != 1 || b2 != 0)
	    __builtin_abort ();
	  break;
	}
    }
  return 0;
}
