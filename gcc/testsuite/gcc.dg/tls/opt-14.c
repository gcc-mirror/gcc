/* This testcase generated invalid assembly on ARM Thumb-2.  Two
   PIC additions of pc were combined, but the deleted label was still
   used.  */
/* { dg-do assemble } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls_native } */

struct __res_state
{
  int options;
};
extern __thread struct __res_state *__resp
  __attribute__ ((tls_model ("initial-exec")));

void foo (void);

int main(void)
{
  int count, total = 0;

  for (count = 0; count < 10; count++)
    {
      if (((*__resp).options & 0x00000001) == 0)
	foo ();
      (*__resp).options &= ~((0x00000002 | 0x00000200 | 0x00000080));
    }
  return 0;
}
