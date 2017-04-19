/* { dg-do compile } */
/* { dg-require-effective-target tls } */

extern int omp_get_num_threads (void);
int x, y, t, z[1000];
#pragma omp threadprivate(x)
void
a24 (int a)
{
  const int c = 1;
  int i = 0;
  int l = 0;
#pragma omp parallel default(none) private(a) shared(z)
  {
    int j = omp_get_num_threads ();
    /* O.K. - j is declared within parallel region */
    /* O.K.  -  a is listed in private clause */
    /*       -  z is listed in shared clause */
    x = c;			/* O.K.  -  x is threadprivate */
    				/*       -  c has const-qualified type */
    z[i] = y;
    /* { dg-error "'i' not specified" "" { target *-*-* } .-1 } */
    /* { dg-error "enclosing 'parallel'" "" { target *-*-* } 13 } */
    /* { dg-error "'y' not specified" "" { target *-*-* } 21 }  */
#pragma omp for firstprivate(y)
    for (i = 0; i < 10; i++)
      {
	z[i] = y;		/* O.K. - i is the loop iteration variable */
				/*      - y is listed in firstprivate clause */
      }
    z[l] = t;
    /* { dg-error "'l' not specified" "" { target *-*-* } .-1 } */
    /* { dg-error "'t' not specified" "" { target *-*-* } .-2 }  */
  }
}
