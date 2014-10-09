/* PR c++/62199 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

int r;
void
foo (int a)
{
  r = a > 0 || !a >= 2; /* { dg-warning "19:logical not is only applied to the left hand side of comparison" } */
  r = !a || a == 10;
  r = !a && !a < 4; /* { dg-warning "16:logical not is only applied to the left hand side of comparison" } */
  r = !a > 0 && a < 6; /* { dg-warning "10:logical not is only applied to the left hand side of comparison" } */
  r = a + (!a < 12); /* { dg-warning "15:logical not is only applied to the left hand side of comparison" } */
  r = a == 7 || !a < 12; /* { dg-warning "20:logical not is only applied to the left hand side of comparison" } */
  r = (a == 7 * a > 0) || !a < 2; /* { dg-warning "30:logical not is only applied to the left hand side of comparison" } */
  r = (1 > !a) || (!42 > a); /* { dg-warning "24:logical not is only applied to the left hand side of comparison" } */
  r = (!5 > a); /* { dg-warning "11:logical not is only applied to the left hand side of comparison" } */
  r = (!0 > a); /* { dg-warning "11:logical not is only applied to the left hand side of comparison" } */
  r = (!-5 > a); /* { dg-warning "12:logical not is only applied to the left hand side of comparison" } */
  r = (!(5 + 3) > a); /* { dg-warning "17:logical not is only applied to the left hand side of comparison" } */
  r = (!(5 - a) > a); /* { dg-warning "17:logical not is only applied to the left hand side of comparison" } */
}
