/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse4" } */
typedef int __v4si __attribute__ ((__vector_size__ (16)));

long long ior_1(__v4si v) {
  unsigned int loVal = (unsigned int)v[0];
  unsigned int hiVal = (unsigned int)v[1];
  return (long long)(loVal) | ((long long)(hiVal) << 32);
}

long long ior_2(__v4si v) {
  unsigned int loVal = (unsigned int)v[2];
  unsigned int hiVal = (unsigned int)v[3];
  return (long long)(loVal) | ((long long)(hiVal) << 32);
}

long long xor_1(__v4si v) {
  unsigned int loVal = (unsigned int)v[0];
  unsigned int hiVal = (unsigned int)v[1];
  return (long long)(loVal) ^ ((long long)(hiVal) << 32);
}

long long xor_2(__v4si v) {
  unsigned int loVal = (unsigned int)v[2];
  unsigned int hiVal = (unsigned int)v[3];
  return (long long)(loVal) ^ ((long long)(hiVal) << 32);
}
/* { dg-final { scan-assembler-not "\torb\t\\\$0," } } */
/* { dg-final { scan-assembler-not "\txorb\t\\\$0," } } */

