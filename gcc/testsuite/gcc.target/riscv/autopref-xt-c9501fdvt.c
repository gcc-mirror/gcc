/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt" } */

void
foo(long *a)
{ 
  a[2] = 1;
  a[1] = 1;
  a[0] = 1;
}

/* { dg-final { scan-assembler "sd.*0\\(a0\\)\n\\s*sd.*8\\(a0\\)\n\\s*sd.*16\\(a0\\)" } } */
