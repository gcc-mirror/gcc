/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-maltivec" } */

/* Program to test the vector_size attribute.  This needs to run on a
   target that has vectors, so use AltiVec.  */

#define vector __attribute__((vector_size(16)))

vector int foobar;

/* Only floats and integrals allowed.  We don't care if they map to SIs.  */
struct X { int frances; };
vector struct X hotdog;	/* { dg-bogus "invalid vector type" "case 1" } */

/* We don't have a V2DF.  */
vector double x;	/* { dg-bogus "no vector mode" "case 2" } */

/* Arrays of vectors.  */
vector char b[10], ouch;

/* Pointers of vectors.  */
vector short *shoe, polish;

int xxx[sizeof(foobar) == 16 ? 69 : -1];

int nc17[sizeof(shoe) == sizeof (char *) ? 69 : -1];

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
