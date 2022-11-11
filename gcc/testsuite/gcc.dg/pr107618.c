/* { dg-do compile } */
/* { dg-options "-Og" } */

void a(void) __attribute__((__warning__("")));
int main(void)
{
  unsigned long b = __builtin_object_size(0, 0);
  if (__builtin_expect(b < 1, 0))
    a(); /* { dg-bogus "warning" } */
}
