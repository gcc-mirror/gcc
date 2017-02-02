/* { dg-do run } */
/* { dg-shouldfail "asan" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

#include <stdlib.h>

typedef __SIZE_TYPE__ size_t;
inline void * operator new (size_t, void *p) { return p; }


struct vec
{
  int size;
};

struct vnull
{
  operator vec() { return vec(); }
};
vnull vNULL;

struct A
{
  A(): value2 (vNULL), value3 (vNULL) {}
  int value;
  vec value2;
  vec value3;
};

int main()
{
  int *array = (int *)malloc (sizeof (int) * 1);
  A *a = new (array) A ();
  free (array);
}

/* { dg-output "ERROR: AddressSanitizer: heap-buffer-overflow.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +in A::A()" } */
