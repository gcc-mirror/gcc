/* Verify zero initialization for structure type automatic variables with
   padding and has explicit initialization.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-rtl-expand -march=x86-64 -mtune=generic -msse -fno-stack-protector" } */

struct test_trailing_hole {
        int one;
        int two;
        int three;
        char four;
        /* "sizeof(unsigned long) - 1" byte padding hole here. */
};

int foo ()
{
  struct test_trailing_hole var = {.one = 1,.two = 2, .four = 'c'};
  return var.four;
}

/* { dg-final { scan-rtl-dump-times "const_int 0 \\\[0\\\]\\\) repeated x16" 1 "expand" { target ia32 } } } */
/* { dg-final { scan-rtl-dump-times "const_int 0 \\\[0\\\]\\\)" 1 "expand" { target { ! ia32 } } } } */
