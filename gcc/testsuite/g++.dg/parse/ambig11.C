// PR c++/64679
// { dg-do run { target c++11 } }

struct Bar {
  int a, b, c;
  Bar(int a, int b, int c) : a(a), b(b), c(c) { }
};

void
f ()
{
  Bar fn1(int(a), int(b), int c = sizeof(a));
  Bar fn2(int(x), int(y), int(z)); // { dg-warning "function declaration" }
  Bar fn3(int(x), int(y), int);
  Bar fn4(int (*p)(int(x), int(y))); // { dg-warning "function declaration" }
  Bar fn5(int (x), int (*p)(int(x), int(y)), int);
}

int
main ()
{
  int x = 1;
  // This ain't a decl.
  Bar v1(int(x), int(x), int{x});
  if (v1.a != 1 || v1.b != v1.a || v1.c != v1.a)
    __builtin_abort ();
  Bar v2(int(x), int(x), 1);
  if (v2.a != 1 || v2.b != v2.a || v2.c != 1)
    __builtin_abort ();
  Bar v3(int(x), int(x), int(1));
  if (v3.a != 1 || v3.b != v3.a || v3.c != 1)
    __builtin_abort ();
  Bar v4(int(1), int(x), int{x});
  if (v4.a != 1 || v4.b != 1 || v4.c != 1)
    __builtin_abort ();
  Bar v5(int{x}, int(x), int{x});
  if (v5.a != 1 || v5.b != v5.a || v5.c != v5.a)
    __builtin_abort ();
}
