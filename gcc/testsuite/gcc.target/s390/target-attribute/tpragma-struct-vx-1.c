/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do run } */
/* { dg-require-effective-target target_attribute } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-march=z900 -mno-vx -mzarch" } */

#define V16 __attribute__ ((vector_size(16)))
#pragma GCC push_options
#pragma GCC target ("arch=z13,vx")
__attribute__ ((noinline))
void foo (char *d, int *off)
{
  typedef struct
  {
    char c;
    V16 char vc;
  } s_t;
  s_t s = { 1,{ 0,11,22,33,44,55,66,77,88,99,101,111,121,131,141,151 }};
  *off = __builtin_offsetof(s_t, vc);
  __builtin_memcpy(d, &s.vc, 16);
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=z13,no-vx")
__attribute__ ((noinline))
void bar (char *d, int *off)
{
  typedef struct
  {
    char c;
    V16 char vc;
  } s_t;
  s_t s = { 1,{ 0,11,22,33,44,55,66,77,88,99,101,111,121,131,141,151 }};
  *off = __builtin_offsetof(s_t, vc);
  __builtin_memcpy(d, &s.vc, 16);
}
#pragma GCC pop_options

int main(int argc, char **argv)
{
  char buf[16] = { 0 };
  char buf2[16] = { 0 };
  int off = 0;
  int off2 = 0;
  int rc;

  rc = 0;
  foo(buf, &off);
  if (off != 8)
    rc += 1;
  if (buf[7] != 77)
    rc += 2;
  bar (buf2, &off2);
  if (off2 != 16)
    rc += 4;
  if (buf2[6] != 66)
    rc += 8;

  return rc;
}
