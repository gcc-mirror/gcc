/* PR tree-optimization/83559 - -Wsuggest-attribute=const conflicts with
   -Wattributes warning about const attribute on function returning void
   { dg-do compile { target nonpic } }
   { dg-options "-O2 -Wsuggest-attribute=pure" } */

int global;

int f_i_v (void)    /* { dg-warning "candidate for attribute .pure." } */
{
  return global;
}

int f_i ()          /* { dg-warning "candidate for attribute .pure." } */
{
  return global;
}

void f_v_v (void)   /* { dg-bogus "candidate" } */
{
}

void f_v ()         /* { dg-bogus "candidate" } */
{
}
