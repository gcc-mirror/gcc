/* { dg-do compile } */
/* { dg-options "-Wtraditional -std=gnu89" } */

int
x (b)
{
  return
  4294967295U   /* { dg-warning "width of integer|traditional C rejects" } */
   / (unsigned long) b;
}
