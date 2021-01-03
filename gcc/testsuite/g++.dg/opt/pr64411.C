// PR target/64411
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } }
// { dg-require-effective-target fpic }
// { dg-options "-Os -mcmodel=medium -fPIC -fschedule-insns -fselective-scheduling" }

typedef __SIZE_TYPE__ size_t;

extern "C"  long strtol ()
  { return 0; }

static struct {
  void *sp[2];
} info;

union S813
{
  void * c[5];
}
s813;

S813 a813[5];
S813 check813 (S813, S813 *, S813);

void checkx813 ()
{
  __builtin_memset (&s813, '\0', sizeof (s813));
  __builtin_memset (&info, '\0', sizeof (info));
  check813 (s813, &a813[1], a813[2]);
}
