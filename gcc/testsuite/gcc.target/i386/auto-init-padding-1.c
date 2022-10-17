/* Verify zero initialization for structure type automatic variables with
   padding.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-rtl-expand -march=x86-64 -mtune=generic -msse" } */

struct test_aligned {
        int internal1;
        long long internal2;
} __attribute__ ((aligned(64)));

int foo ()
{
  struct test_aligned var;
  return var.internal1;
}

/* { dg-final { scan-rtl-dump-times "const_int 0 \\\[0\\\]\\\) repeated x16" 1 "expand" } } */


