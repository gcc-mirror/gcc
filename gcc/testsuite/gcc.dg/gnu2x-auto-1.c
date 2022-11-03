/* Test C2x auto.  Invalid code with GNU extensions.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x" } */

void
f ()
{
  /* Do not allow a non-definition declaration of a tag in the auto
     initializer, to avoid it escaping an inner scope as shown here.  */
  auto x = ({ struct s; struct s *x = 0; x; }); /* { dg-error "declared in underspecified object initializer" } */
}
