/* { dg-do compile { target tail_call } } */
/* Allow nested functions.  */
/* { dg-options "-Wno-pedantic" } */

struct box { char field[64]; int i; };

struct box __attribute__((noinline,noclone))
returns_struct (int i)
{
  struct box b;
  b.i = i * i;
  return b;
}

int __attribute__((noinline,noclone))
test_1 (int i)
{
  return returns_struct (i * 5).i; /* { dg-error "cannot tail-call: " } */
}

int __attribute__((noinline,noclone))
test_2_callee (int i, struct box b)
{
  if (b.field[0])
    return 5;
  return i * i;
}

int __attribute__((noinline,noclone))
test_2_caller (int i)
{
  struct box b;
  return test_2_callee (i + 1, b); /* { dg-error "cannot tail-call: " } */
}

extern void setjmp (void);
void
test_3 (void)
{
  setjmp (); /* { dg-error "cannot tail-call: " } */
}

void
test_4 (void)
{
  void nested (void)
  {
  }
  nested (); /* { dg-error "cannot tail-call: " } */
}

typedef void (fn_ptr_t) (void);
volatile fn_ptr_t fn_ptr;

void
test_5 (void)
{
  fn_ptr ();
}
