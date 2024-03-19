/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that, along with a strub const function call, we issue an asm
   statement to make sure the watermark passed to it is held in memory before
   the call, and another to make sure it is not assumed to be unchanged.  f
   should not be inlined into g, but if it were too simple it might be folded
   by interprocedural value-range propagation.  */

extern int __attribute__ ((__strub__ ("callable"),
			   __const__, __nothrow__)) c ();

int __attribute__ ((__strub__, __const__))
f () {
  return c ();
}

int
g () {
  return f ();
}

/* { dg-final { scan-ipa-dump-times "__asm__" 2 "strub" } } */
