/* { dg-do compile } */
/* { dg-options "-O3" } */

int bar (int);
void car (long *, int *, int);
int 
foo( long * np, int * dp, int qn)
{
  int i;
  int n0;
  int d0;
  int a;
  int b;
  int c;
  int d;

  a = 1;
  b = 0;
  c = 1;
  d = 1;

  d0 = dp[0];

  for (i = qn; i >= 0; i--) {
    if (bar((c == 0)) && (np[1] == d0)) {
      car(np - 3, dp, 3);
    } else { 
      __asm__ ("xma.hu %0 = %2, %3, f0\n\txma.l %1 = %2, %3, f0" : "=&f" ((a)), 
"=f" (b) : "f" ((c)), "f" ((d))); 
      n0 = np[0]; 
      if (n0 < d0) 
        c = 1; 
      else 
        c = 0; 

    }
    *--np = a;
  }

  return 0;
}
