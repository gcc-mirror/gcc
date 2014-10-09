/* typeof applied to const+nonconst should be nonconst, as should
   typeof applied to other arithmetic expressions.  Bug 13519.  */
/* Origin: Debian bug report 208981
   from Kalle Olavi Niemitalo <kon@iki.fi>, adapted to a testcase by
   Joseph Myers <jsm@polyomino.org.uk>.  */
/* { dg-do compile } */
/* { dg-options "" } */

void fn(void)
{
  int n;
  const int c;

  { __typeof__(n) a1; a1=0; }
  { __typeof__(c) a2; a2=0; } /* { dg-error "read-only" "correct error" } */
  { __typeof__((int)n) a3; a3=0; }
  { __typeof__((int)c) a4; a4=0; } /* { dg-bogus "read-only" "bogus error" } */
  { __typeof__((const int)n) a5; a5=0; }
  { __typeof__((const int)c) a6; a6=0; }
  { __typeof__(0) a7; a7=0; }
  { __typeof__(1) a8; a8=0; }

  { __typeof__(n+n) b0; b0=0; }
  { __typeof__(n+c) b1; b1=0; }
  { __typeof__(c+n) b2; b2=0; }
  { __typeof__(c+c) b3; b3=0; }

  { __typeof__(0+n) c0; c0=0; }
  { __typeof__(0+c) c1; c1=0; }
  { __typeof__(n+0) c2; c2=0; }
  { __typeof__(c+0) c3; c3=0; }

  { __typeof__(1+n) d0; d0=0; }
  { __typeof__(1+c) d1; d1=0; }
  { __typeof__(n+1) d2; d2=0; }
  { __typeof__(c+1) d3; d3=0; }

  { __typeof__(((int)n)+((int)n)) e0; e0=0; }
  { __typeof__(((int)n)+((int)c)) e1; e1=0; }
  { __typeof__(((int)c)+((int)n)) e2; e2=0; }
  { __typeof__(((int)c)+((int)c)) e3; e3=0; }

  { __typeof__(((const int)n)+((const int)n)) f0; f0=0; }
  { __typeof__(((const int)n)+((const int)c)) f1; f1=0; }
  { __typeof__(((const int)c)+((const int)n)) f2; f2=0; }
  { __typeof__(((const int)c)+((const int)c)) f3; f3=0; }
}
