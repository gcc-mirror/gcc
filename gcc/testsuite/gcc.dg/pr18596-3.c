/* { dg-do compile } */
/* { dg-options "" } */

int foo ()
{
  static g () = 0; /* { dg-error "invalid storage class" } */
  static int f () = 1; /* { dg-error "invalid storage class" } */
  auto int h () = 0; /* { dg-error "initialized like a variable" } */
  static int i () = { 0 }; /* { dg-error "invalid storage class" } */
  static int j () = /* { dg-error "invalid storage class" } */
	{ 0, 0.0 };
}
/* { dg-warning "excess elements" "" { target *-*-* } 11 } */
/* { dg-warning "near initialization" "" { target *-*-* } 11 } */
