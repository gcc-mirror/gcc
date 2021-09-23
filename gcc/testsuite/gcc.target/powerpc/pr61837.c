/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-loop2_doloop -fno-unroll-loops" } */
/* The inner loop would use the doloop IV in word_mode.  And then
   there is no need to access it though zero_extend on shorter mode.  */
void foo(int *p1, long *p2, int s)
{
  int n, v, i;

  v = 0;
  for (n = 0; n <= 100; n++) {
     for (i = 0; i < s; i++)
        if (p2[i] == n)
           p1[i] = v;
     v += 88;
  }
}

/* { dg-final {scan-rtl-dump-not {(?p)zero_extend.*doloop} "loop2_doloop"} } */
/* { dg-final {scan-rtl-dump-not {(?p)reg:SI.*doloop} "loop2_doloop" { target lp64 } } } */

