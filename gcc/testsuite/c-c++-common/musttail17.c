/* { dg-do compile { target musttail } } */

struct box { char field[64]; int i; };

struct box __attribute__((noinline,noclone,noipa))
returns_struct (int i)
{
  struct box b;
  b.i = i * i;
  return b;
}

int __attribute__((noinline,noclone))
test_1 (int i)
{
  __attribute__((musttail)) return returns_struct (i * 5).i; /* { dg-error "cannot tail-call: " } */
}
