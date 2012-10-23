/* { dg-do compile { target aarch64*-*-* } } */

struct S
{
  union
    {
      long double b;
    } a;
};

struct S s;

extern struct S a[5];
extern struct S check (struct S, struct S *, struct S);
extern void checkx (struct S);

void test (void)
{
  checkx (check (s, &a[1], a[2]));
}
