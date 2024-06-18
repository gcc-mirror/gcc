/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Program to test the vector_size attribute.  This needs to run on a
   target that has vectors, so use AltiVec.  */

#define vector __attribute__((vector_size(16)))

vector int foobar;

/* Only floats and integrals allowed.  We don't care if they map to SIs.  */
struct X { int frances; };
vector struct X hotdog;	/* { dg-error "invalid vector type" } */

/* Arrays of vectors.  */
vector char b[10], ouch;

/* Pointers of vectors.  */
vector short *shoe, polish;

int xxx[sizeof(foobar) == 16 ? 69 : -1];

int nc17[sizeof(shoe) == sizeof (char *) ? 69 : -1];

void
code ()
{
  *shoe = polish;
  b[1] = ouch;
}

vector short
hoop ()
{
  return polish;
}
