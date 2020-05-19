/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */

typedef int int32_t __attribute__((mode (__SI__)));
typedef unsigned uint32_t __attribute__((mode (__SI__)));

int32_t foo () {
  int32_t i;
  uint32_t* pu = reinterpret_cast<uint32_t*> (&i);  /* { dg-bogus "signed vs. unsigned" } */
  *pu = 1000000;
  return i;
}
