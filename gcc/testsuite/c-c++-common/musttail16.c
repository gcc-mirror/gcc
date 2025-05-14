/* { dg-do compile { target musttail } } */

struct box { char field[256]; int i; };

int __attribute__((noinline,noclone,noipa))
test_2_callee (int i, struct box b)
{
  if (b.field[0])
    return 5;
  return i * i;
}

int __attribute__((noinline,noclone,noipa))
test_2_caller (int i)
{
  struct box b;
  __attribute__((musttail)) return test_2_callee (i + 1, b); /* { dg-error "cannot tail-call: " } */
}

extern void setjmp (void);
void
test_3 (void)
{
  __attribute__((musttail)) return setjmp (); /* { dg-error "cannot tail-call: " } */
}

extern float f7(void);

int
test_6 (void)
{
  __attribute__((musttail)) return f7(); /* { dg-error "cannot tail-call: " } */
}
