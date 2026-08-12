/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt" } */

/* Leave gaps between stores so that load/store pair fusion does not constrain
   the order chosen by the autoprefetcher model.  */

void
foo (long *a)
{
  a[4] = 1;
  a[2] = 1;
  a[0] = 1;
}

/* { dg-final { scan-assembler "sd.*0\\(a0\\)\n\\s*sd.*16\\(a0\\)\n\\s*sd.*32\\(a0\\)" } } */
