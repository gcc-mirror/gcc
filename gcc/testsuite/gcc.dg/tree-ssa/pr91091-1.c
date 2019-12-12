/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing" } */

struct s { __INT32_TYPE__ x; } __attribute__((packed));
struct t { __INT32_TYPE__ x; };

void __attribute__((noinline,noipa))
swap(struct s* p, struct t* q)
{
  p->x = q->x;
  q->x = p->x;
}

int main()
{    
  struct t a[2];
  a[0].x = 0x12345678;
  a[1].x = 0x98765432;
  swap ((struct s *)((char *)a + 1), a);
  if (a[0].x != 0x12345678)
    __builtin_abort ();
  return 0;
}
