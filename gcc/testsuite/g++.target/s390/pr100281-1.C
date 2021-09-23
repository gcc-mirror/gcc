// PR C++/100281
// { dg-do compile }

typedef void * __attribute__((mode (SI))) __ptr32_t;

void foo () {
  unsigned int b = 100;
  __ptr32_t a;
  a = b; /* { dg-error "invalid conversion from 'unsigned int' to '__ptr32_t'.*" } */
}
