/* We were removing the label "a" because
   we were removing the BB for it but forgot
   to add back the label.   */

/* { dg-options "" } */

void
f (void)
{
  static __SIZE_TYPE__ x = &&a - &&b;
  a : b : return;
}
