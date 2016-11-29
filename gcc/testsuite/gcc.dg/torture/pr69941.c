/* { dg-do run } */

#if __SIZEOF_INT__ < 4
__extension__ typedef __INT32_TYPE__ int32_t;
#else
typedef int int32_t;
#endif
 
int a = 0;
int b = 0;
int c = 0;
int32_t e = 0;
int f = 0;
int32_t *g = &e;
 
int fn1() { return b ? a : b; }
 
int main() {
  int h = fn1() <= 0x8000000000000000ULL; // h = 1;
 
  int k = f; // k = 0;
 
  long i = h ? k : k / h; // i = 0;
 
  long l = (unsigned short)(i - 0x1800); // l = 0xe800
 
  i = l ? l : c; // i = 0xe800;
 
  *g = i; // *g = 0xe800; e = 0xe800;
 
  unsigned char result = e >> 9; // result = 0x74;

  if ((int)result != 0x74)
    __builtin_abort ();
  return 0;
}
