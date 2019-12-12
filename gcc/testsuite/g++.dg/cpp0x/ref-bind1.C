// PR c++/91844 - Implement CWG 2352, Similar types and reference binding.
// { dg-do compile { target c++11 } }

// These should bind directly to ptr, so no -Wreturn-local-addr warnings.
int *ptr;

const int *const &
fn1 ()
{
  return ptr;
}

int **const ptr2 = nullptr;
const int *const *const &
fn2 ()
{
  return ptr2;
}

int (*ptr3)[10];
using T = const int (*const)[10];

T&
fn3 ()
{
  return ptr3;
}

int (**ptr4)[5] = nullptr;
using T2 = const int (*const *const)[5];

T2&
fn4 ()
{
  return ptr4;
}

const int **ptr5 = nullptr;

const int *const *const &
fn5 ()
{
  return ptr5;
}
