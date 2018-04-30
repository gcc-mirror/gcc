/* PR tree-optimization/83559 - -Wsuggest-attribute=const conflicts with
   -Wattributes warning about const attribute on function returning void
   { dg-do compile { target nonpic } }
   { dg-options "-O2 -Wsuggest-attribute=const" } */

int f_i_v (void)    /* { dg-warning "candidate for attribute .const." } */
{
  return 0;
}

int f_i ()          /* { dg-warning "candidate for attribute .const." } */
{
  return 0;
}

void f_v_v (void)   /* { dg-bogus "candidate" } */
{
}

void f_v ()         /* { dg-bogus "candidate" } */
{
}
