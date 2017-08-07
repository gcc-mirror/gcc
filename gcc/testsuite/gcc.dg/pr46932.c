/* { dg-options "-O2 -fdump-rtl-auto_inc_dec" } */

/* Build on targets which have pre increment.  */
/* { dg-do compile { target aarch64*-*-* arm*-*-* rs6000-*-* powerpc*-*-* arc*-*-* m32r-*-* tic6x-*-* } } */

/* Check that accesses based on the frame pointer do not
   use auto increment.  */

extern void foo(char*);
void t01(char t)
{
  char c = t;
  foo(&c);
}

/* { dg-final { scan-rtl-dump-not "success" "auto_inc_dec" } } */
