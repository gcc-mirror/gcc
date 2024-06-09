// { dg-do compile }

#include <cstdio>
#include <cstring>
#include <cassert>

typedef struct {
  int arr[100];
} S;

int main()
{
  S *s = new S;

  memset (s->arr, '\0', sizeof s->arr); 

#pragma omp target enter data map(to: (*s).arr)
  /* You can't do this, at least as of OpenMP 5.2.  "has_device_addr" takes
     a "variable list" item type
     (OpenMP 5.2, "5.4.9 has_device_addr Clause").  */
#pragma omp target has_device_addr((*s).arr[5:20])
// { dg-error {expected unqualified-id before '\(' token} "" { target *-*-* } .-1 }
  {
    for (int i = 5; i < 25; i++)
      s->arr[i] = i; 
  }

#pragma omp target exit data map(from: (*s).arr)

  for (int i = 0; i < 100; i++)
    assert (i >= 5 && i < 25 ? s->arr[i] == i : s->arr[i] == 0);

  delete s;

  return 0;
}
