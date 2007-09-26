/* { dg-do run } */
/* { dg-options "--param max-aliased-vops=0" } */

typedef struct _s {
    int a;
    int b;
    int c;
    int d;
} s;

extern void abort(void);

void __attribute__((noinline)) g(s *p)
{
  if (p->d != 0)
    abort ();
}

char *c = (void*)0;
void __attribute__((noinline)) f(void) { if (c) *c = 1; }

void test_signed_msg_encoding(void)
{
    s signInfo = { sizeof(signInfo), 0 };

    signInfo.b = 1;
    signInfo.c = 0;
    g(&signInfo);
    signInfo.d = 1;
    f();
}

int main()
{
  test_signed_msg_encoding ();
  test_signed_msg_encoding ();
  return 0;
}
