#include <string.h>

int main()
{
  char s[5]; /* { dg-message "region created on stack here" } */
  memmove(s, s + 1, 2); /* { dg-warning "use of uninitialized value" } */
  return 0;
}
