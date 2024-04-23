/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class -fsyntax-only" } */

__attribute__((objc_nullability(0))) id a;
__attribute__((objc_nullability(4))) id e_1; /* { dg-error {'objc_nullability' attribute argument '4' is not an integer constant between 0 and 3} } */
__attribute__((objc_nullability(-22))) id e_2; /* { dg-error {'objc_nullability' attribute argument '-22' is not an integer constant between 0 and 3} } */
__attribute__((objc_nullability("unspecified"))) id b;
__attribute__((objc_nullability("nullable"))) id c;
__attribute__((objc_nullability("nonnull"))) id d;
__attribute__((objc_nullability("resettable"))) id e;
__attribute__((objc_nullability("nonsense"))) id e_3; /* { dg-error {'objc_nullability' attribute argument '"nonsense"' is not recognized} } */
__attribute__((objc_nullability(noGoingToWork))) id e_4; /* { dg-error {'noGoingToWork' undeclared here} } */

@interface MyRoot
{
  __attribute__((objc_nullability(0))) id iv_a;
  __attribute__((objc_nullability(3))) struct { int bad_a; } s;/* { dg-error {'objc_nullability' cannot be applied to non-pointer type 'struct <anonymous>'} } */
  __attribute__((objc_nullability("resettable"))) int iv_b;/* { dg-error {'objc_nullability' cannot be applied to non-pointer type 'int'} } */
}
@end
