/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mabi=sysv -march=diamondrapids -mapxf -fdump-rtl-ira" } */

/* The call is on a cold path, so the moves on the loop border cost more
   than the saves around it.  */

extern void slow (long *) __attribute__((cold));

long
scan (long *p, long n, long *q)
{
  long a = q[0], b = q[1], c = q[2], d = q[3];
  long e = q[4], f = q[5], g = q[6], h = q[7];
  long s = 0;
  for (long i = 0; i < n; i++)
    {
      s += p[i];
      if (__builtin_expect (p[i] < 0, 0))
	slow (p);
    }
  return s + a + b + c + d + e + f + g + h;
}

/* { dg-final { scan-rtl-dump {Loop 1 flatten-cost:[^\n]*bias=-[0-9]+ \(allow flatten\)} "ira" } } */
/* { dg-final { scan-rtl-dump-not {flatten-cost:[^\n]*keep} "ira" } } */
