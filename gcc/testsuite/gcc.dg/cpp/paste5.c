/* Regression test for bug in convoluted situation involving token paste
   plus function-like macros used outside function context.  It may be
   easier to understand if you mentally replace 'struct' with 'A'
   throughout this file; 'struct' is used only to get the code to compile
   when preprocessed correctly.

   The original problem was seen in the Linux kernel and reported by
   Jakub Jelinek <jakub@redhat.com>; this test is synthetic.  */

/* { dg-do compile } */

#define glue(a,b) a##b
#define struct(x) B(x)
#define E(x) struct x
#define FG (22)

extern void B(int);

void foo(void)
{
  E(glue(F,*)) dummy;  /* { dg-error "valid preprocessing token" } */

  E(glue(F,G)) ;
}
