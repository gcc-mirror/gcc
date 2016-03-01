/* { dg-options "-Wformat" } */
extern int printf (const char *__restrict __format, ...);
void test (void)
{
  /* A very long line, so that we start a new line map.  */
  printf ("%llu01233456789012334567890123345678901233456789012334567890123345678901233456789012334567890123345678901233456789012334567890123345678901233456789"); /* { dg-warning "15: format .%llu. expects a matching" } */
}
