/* { dg-do compile } */

struct S {
  signed char *ptr;
};

int main()
{
  signed char arr[20];

  /* Reject array section in compound initialiser.  */
#pragma omp target map( (struct S) { .ptr = (signed char *) arr[5:5] } )
/* { dg-error {expected '\]' before ':' token} "" { target *-*-* } .-1 } */
/* { dg-warning {cast to pointer from integer of different size} "" { target *-*-* } .-2 } */
/* { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-3 } */
  { }

  /* ...and this is unsupported too (probably not useful anyway).  */
#pragma omp target map( (struct S) { .ptr = &arr[5] } )
/* { dg-message {sorry, unimplemented: unsupported map expression} "" { target *-*-* } .-1 } */
  { }

  return 0;
}
