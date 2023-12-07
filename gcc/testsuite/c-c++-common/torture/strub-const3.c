/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */
/* { dg-require-effective-target strub } */

/* Check that, along with a strub const wrapping call, we issue an asm statement
   to make sure the watermark passed to it is held in memory before the call,
   and another to make sure it is not assumed to be unchanged.  */

extern int __attribute__ ((__strub__ ("callable"),
			   __const__, __nothrow__)) c ();

int __attribute__ ((__strub__ ("internal"), __const__))
f () {
  return c ();
}

/* { dg-final { scan-ipa-dump-times "__asm__" 2 "strub" } } */
