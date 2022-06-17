#include <stdlib.h>
struct s {
  char *p;
  int arr[1];
};
int main(void) {
  struct s s;
  s.p = malloc(1);
  for (int i = 0; i < 1; i++)
    s.arr[i] = -1; /* { dg-bogus "leak" } */
  free(s.p);
}
