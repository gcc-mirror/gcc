/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef int __attribute__((vector_size(8))) v2si_t;
typedef int __attribute__((vector_size(16))) v4si_t;
typedef double __attribute__((vector_size(16))) v2df_t;

struct __attribute__((packed)) s {
  int i;
  v2si_t m;
  v4si_t v;
};

int test (struct s*ps)
{
  v4si_t r = ps->v;
  v2si_t m;

  if (ps->i > 0)
    {
      asm volatile ("" : "+m" (*ps));
      m = __builtin_ia32_cvtpd2pi ((v2df_t)ps->v);
      r[0] = __builtin_ia32_paddd (m, m)[0];
    }
  else
    {
      asm volatile ("" : "+m" (*ps));
      m = __builtin_ia32_cvttpd2pi ((v2df_t)ps->v);
      r[0] = __builtin_ia32_paddd (m, m)[0];
    }

  return r[0];
}

/* { dg-final { scan-assembler-not "cvtpd2pi\[ \t]\[^\n\r]*\\(" } } */
/* { dg-final { scan-assembler-not "cvttpd2pi\[ \t]\[^\n\r]*\\(" } } */
