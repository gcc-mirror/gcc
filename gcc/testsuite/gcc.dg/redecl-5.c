/* Test for multiple declarations and composite types.  Diagnosis of
   incompatible implicit declaration.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=c89" } */

void
f (void)
{
  long z(); /* { dg-message "note: previous implicit declaration" } */
}

void
g (void)
{
  z(); /* { dg-error "incompatible" } */
  labs(1); /* { dg-warning "incompatible" } */
  printf("x"); /* { dg-warning "incompatible" } */
}
