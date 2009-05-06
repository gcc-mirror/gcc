/* ICE on unnamed field with incomplete enum type: PR 40032.  */
/* { dg-do compile } */
/* { dg-options "" } */
struct A
{
  enum E : 8; /* { dg-warning "narrower than values of its type" } */
  /* { dg-error "has incomplete type" "incomplete" { target *-*-* } 6 } */
};
