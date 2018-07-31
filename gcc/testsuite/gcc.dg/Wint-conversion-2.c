/* PR middle-end/86202 */
/* { dg-do compile } */
/* { dg-options "-Wint-conversion" } */

void *memcpy (void *, void *, __SIZE_TYPE__ *);
void *a, *b;
void f (void)
{
  long unsigned int c = 0;
  memcpy (a, b, c); /* { dg-warning "passing argument" } */
}
