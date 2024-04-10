/* Verify we don't ICE on cases involving very large values for size.  */

char s, d;

void
foo(void)
{
  __builtin_strncpy(&d, &s + 3, -1); /* { dg-warning "Wstringop-overflow" } */
  __builtin_strncpy(&d + 3, &s, -1); /* { dg-warning "Wstringop-overflow" } */
}

void
bar(void)
{
  __builtin_strncpy(&d, &s - 3, -1); /* { dg-warning "Wstringop-overflow" } */
  __builtin_strncpy(&d - 3, &s, -1); /* { dg-warning "Wstringop-overflow" } */
}
