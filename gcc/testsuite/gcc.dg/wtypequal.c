/* { dg-do compile } */

int *__restrict *p;
void f(void)
{
  __typeof(*p) *q = p; /* { dg-bogus "discards qualifiers" } */
}
