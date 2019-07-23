typedef long int jmp_buf[8];
extern
#ifdef __cplusplus
"C"
#endif
int setjmp (jmp_buf);

void
foo (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < 64; i++)
    {
      jmp_buf buf;
      setjmp (buf);	/* { dg-error "setjmp/longjmp inside 'simd' construct" } */
    }
}

void
bar (void)
{
  int i;
  #pragma omp loop bind(thread)
  for (i = 0; i < 64; i++)
    {
      jmp_buf buf;
      setjmp (buf);
    }
}

#ifdef __cplusplus
struct S
{
  static int setjmp (jmp_buf);
};

namespace N
{
  int setjmp (jmp_buf);
}

void
baz (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < 64; i++)
    {
      jmp_buf buf;
      S::setjmp (buf);
      N::setjmp (buf);
    }
}

void
qux (void)
{
  int i;
  #pragma omp loop bind(thread)
  for (i = 0; i < 64; i++)
    {
      jmp_buf buf;
      S::setjmp (buf);
      N::setjmp (buf);
    }
}
#endif
