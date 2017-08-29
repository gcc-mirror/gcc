/* { dg-options "-O2 -fdump-rtl-auto_inc_dec" } */
/* { dg-require-effective-target autoincdec } */

/* Check that accesses based on the frame pointer do not
   use auto increment.  */

extern void foo(char*);
void t01(char t)
{
  char c = t;
  foo(&c);
}

/* { dg-final { scan-rtl-dump-not "success" "auto_inc_dec" } } */
