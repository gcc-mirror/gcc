// { dg-do assemble  }
// GROUPS passed bad-errors
#include <stddef.h>

void * operator new(size_t, int *);
void * operator new(size_t, void *);

int *x = 0;
int foo(){
new (x) int *;
new (&x) int *;
new (x) int *;  // This is identical to line 8 !!!
return 1;
}
