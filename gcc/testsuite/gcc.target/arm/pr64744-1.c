/* { dg-do compile } */
/* { dg-require-effective-target naked_functions } */
/* { dg-options "-O0" } */

__attribute__((naked))
void foo1 ()
{
  int aa = 0;
  int ab = {0};
}

__attribute__((naked))
void foo2() {
  char aa [ ] = {}; /* { dg-error "cannot allocate stack for variable" } */
  char ab [1] = {};
  char ac [2] = {}; /* { dg-error "cannot allocate stack for variable" } */
  char ad [3] = {}; /* { dg-error "cannot allocate stack for variable" } */
}

__attribute__((naked))
void foo3() {
  char aa [1] = {0};
  char ab [2] = {0}; /* { dg-error "cannot allocate stack for variable" } */
  char ac [3] = {0}; /* { dg-error "cannot allocate stack for variable" } */
  char ad [4] = {0}; /* { dg-error "cannot allocate stack for variable" } */
}

__attribute__((naked))
void foo4() {
  char aa [2] = {0,0}; /* { dg-error "cannot allocate stack for variable" } */
}
__attribute__((naked))
void foo5() {
  char aa [3] = {0,0,0}; /* { dg-error "cannot allocate stack for variable" } */
}

__attribute__((naked))
void foo6() {
  char aa [4] = {0,0,0,0}; /* { dg-error "cannot allocate stack for variable" } */
}
