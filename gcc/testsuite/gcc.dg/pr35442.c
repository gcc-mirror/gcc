/* PR c/35442 */
/* { dg-bogus "not supported by" "" { target *-*-* } 0 } */

typedef char A __attribute__ ((vector_size (64)));
typedef int B __attribute__ ((vector_size (64)));

void
foo (A a)
{
  ((B) a) ();	/* { dg-error "is not a function" } */
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector passed by reference.*" } */
