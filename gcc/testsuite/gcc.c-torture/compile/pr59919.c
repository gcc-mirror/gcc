typedef int jmp_buf[10];
struct S
{
  int i;
  jmp_buf buf;
};

void setjmp (jmp_buf);
void foo (int *);
__attribute__ ((__noreturn__, __nonnull__)) void bar (struct S *);

void
baz (struct S *p)
{
  bar (p);
  setjmp (p->buf);
  foo (&p->i);
}
