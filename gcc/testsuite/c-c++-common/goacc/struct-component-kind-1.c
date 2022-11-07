/* { dg-do compile } */

#include <stdlib.h>

#define N 20

struct s {
  int base[N];
};

int main (void)
{
  struct s v;

#pragma acc parallel copy(v, v.base[0:N])
{ }

#pragma acc parallel copyin(v, v.base[0:N])
{ }

#pragma acc parallel copyout(v, v.base[0:N])
{ }

#pragma acc parallel copy(v) copyin(v.base[0:N])
{ }

#pragma acc parallel copy(v) copyout(v.base[0:N])
{ }

#pragma acc parallel copy(v) present(v.base[0:N])
{ }

#pragma acc parallel copyin(v) present(v.base[0:N])
{ }

#pragma acc parallel copyout(v) present(v.base[0:N])
{ }

#pragma acc enter data copyin(v, v.base[0:N])
#pragma acc update device(v, v.base[0:N])
#pragma acc exit data delete(v, v.base[0:N])

#pragma acc parallel copyin(v) copy(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

#pragma acc parallel copyout(v) copy(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

#pragma acc parallel present(v) copy(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

#pragma acc parallel present(v) copyin(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

#pragma acc parallel present(v) copyout(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

#pragma acc parallel present(v) no_create(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

#pragma acc parallel no_create(v) present(v.base[0:N])
/* { dg-error "data movement for component 'v\\.(s::)?base\\\[0\\\]' is not compatible with movement for struct 'v'" "" { target *-*-* } .-1 } */
{ }

  return 0;
}
