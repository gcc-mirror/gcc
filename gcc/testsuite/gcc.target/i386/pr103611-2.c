/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse4" } */

typedef int __v4si __attribute__ ((__vector_size__ (16)));

long long test1(__v4si v) {
  unsigned int loVal = (unsigned int)v[0];
  unsigned int hiVal = (unsigned int)v[1];
  return (long long)(loVal) | ((long long)(hiVal) << 32);
}

long long test2(__v4si v) {
  unsigned int loVal = (unsigned int)v[2];
  unsigned int hiVal = (unsigned int)v[3];
  return (long long)(loVal) | ((long long)(hiVal) << 32);
}

long long test3(__v4si v) {
  unsigned int loVal = (unsigned int)v[0];
  unsigned int hiVal = (unsigned int)v[1];
  return (long long)(loVal) ^ ((long long)(hiVal) << 32);
}

long long test4(__v4si v) {
  unsigned int loVal = (unsigned int)v[2];
  unsigned int hiVal = (unsigned int)v[3];
  return (long long)(loVal) ^ ((long long)(hiVal) << 32);
}

long long test5(__v4si v) {
  unsigned int loVal = (unsigned int)v[0];
  unsigned int hiVal = (unsigned int)v[1];
  return (long long)(loVal) + ((long long)(hiVal) << 32);
}

long long test6(__v4si v) {
  unsigned int loVal = (unsigned int)v[2];
  unsigned int hiVal = (unsigned int)v[3];
  return (long long)(loVal) + ((long long)(hiVal) << 32);
}

/* { dg-final { scan-assembler-not "\tor" } } */
/* { dg-final { scan-assembler-not "\txor" } } */
/* { dg-final { scan-assembler-not "\tadd" } } */
