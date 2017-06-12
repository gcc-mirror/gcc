/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

int foo ()
{
  static g () = 0; /* { dg-error "invalid storage class" } */
  static int f () = 1; /* { dg-error "invalid storage class" } */
  auto int h () = 0; /* { dg-error "initialized like a variable" } */
  /* { dg-error "declared but never defined" "nested" { target *-*-* } .-1 } */
  static int i () = { 0 }; /* { dg-error "invalid storage class" } */
  static int j () = /* { dg-error "invalid storage class" } */
	{ 0, 0.0 };
}
