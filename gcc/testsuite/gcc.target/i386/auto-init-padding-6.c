/* Verify pattern initialization for structure type automatic variables with
   tail padding.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-rtl-expand -march=x86-64 -mtune=generic -msse" } */

struct test_trailing_hole {
        char *one;
        char *two;
        char *three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};

int foo ()
{
  struct test_trailing_hole var;
  return var.four;
}

/* { dg-final { scan-rtl-dump-times "0xfffffffffffffffe\\\]\\\) repeated x16" 1 "expand" } } */


