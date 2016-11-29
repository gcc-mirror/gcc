// { dg-do compile { target c++11 } }

struct A {int i;}  a [[gnu::aligned(16)]];
struct B {int i;} __attribute__((aligned(16))) b;

constexpr unsigned si = sizeof(int);
constexpr unsigned ai = alignof(int);

int
main ()
{
 A aa;
 B bb;

 static_assert (sizeof (a) == si, "sizeof (a) should be 4");
 static_assert (sizeof (b) == 16, "sizeof (b) should be 16");
 static_assert (sizeof (aa) == si, "sizeof (aa) should be 4");
 static_assert (sizeof (bb) == 16, "sizeof (bb) should be 16");

 static_assert (__alignof__  (a) == 16, "alignof (a) should be 16");
 static_assert (__alignof__  (b) == 16, "alignof (b) should be 16");
 static_assert (__alignof__  (aa) == ai, "alignof (aa) should be 4");
 static_assert (__alignof__  (bb) == 16, "alignof (bb) should be 16");
}
