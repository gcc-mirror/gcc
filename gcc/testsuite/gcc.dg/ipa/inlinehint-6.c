/* { dg-options "-O3 -c -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-cp"  } */
/* { dg-add-options bind_pic_locally } */

#define size_t long long int

struct A
{
  size_t f1, f2, f3, f4;
};
struct C
{
  struct A a;
  size_t b;
};
struct C x;

__attribute__((hot)) struct C callee (struct A *a, struct C *c)
{
  c->a=(*a);

  if((c->b + 7) & 17)
   {
      c->a.f1 = c->a.f2 + c->a.f1;
      c->a.f2 = c->a.f3 - c->a.f2;
      c->a.f3 = c->a.f2 + c->a.f3;
      c->a.f4 = c->a.f2 - c->a.f4;
      c->b = c->a.f2;

    }
  return *c;
}

__attribute__((hot)) struct C caller (size_t d, size_t e, size_t f, size_t g, struct C *c)
{
  struct A a;
  a.f1 = 1 + d;
  a.f2 = e;
  a.f3 = 12 + f;
  a.f4 = 68 + g;
  if (c->b > 0)
    return callee (&a, c);
  else
    return *c;
}

/* { dg-final { scan-ipa-dump "known_hot"  "inline"  } } */

