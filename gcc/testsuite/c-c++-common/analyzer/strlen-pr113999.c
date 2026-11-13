#include "analyzer-decls.h"

int main() {
  char a[20] = "ab";
  __analyzer_eval (__builtin_strlen(a) == 2); // { dg-warning "TRUE" }
  __builtin_strcpy(a, "123");
  long n0 = __builtin_strlen(a);
  __analyzer_eval (n0 == 3); // { dg-warning "TRUE" }
  __builtin_strncpy(a + 3, a, n0);
  __analyzer_eval (__builtin_strlen(a) == 6); // { dg-warning "TRUE" }
  return 0;
}
