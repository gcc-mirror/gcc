/* Test AAPCS layout

/* { dg-do compile { target aarch64*-*-* } } */

#define vector __attribute__((vector_size(16)))

void
foo(int a, ...);

int
main(void)
{
  foo (1, (vector unsigned int){10,11,12,13},
       2, (vector unsigned int){20,21,22,23});
  return 0;
}
