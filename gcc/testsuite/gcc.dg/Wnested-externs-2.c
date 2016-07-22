/* PR c/70791 */
/* { dg-do compile } */
/* { dg-options "-Wnested-externs" } */

void
bar (void)
{
  extern int i; /* { dg-warning "14:nested extern declaration of 'i'" } */
  extern short foo (void); /* { dg-warning "16:nested extern declaration of 'foo'" } */
  extern struct S *s; /* { dg-warning "20:nested extern declaration of 's'" } */
}
