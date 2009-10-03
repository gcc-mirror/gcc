// { dg-lto-do assemble }
extern void some_function (const char *);
extern bool some_other_function ();

struct Foo
{
 long long a;
 int b;
};

bool Foo_eq(Foo x, Foo y)
{
 return x.a == y.a && x.b == y.b;
}

struct Bar
{
 Foo a;
 int b;
};

struct Baz
{
 Bar a;
 Baz(Bar &a):a(a) { }
};

struct Zonk
{
 Baz baz;

 Bar func_1(const Bar & bar) {
   if (Foo_eq(bar.a, baz.a.a) && bar.b == baz.a.b || some_other_function ())
     return bar;
 }

 void func_2(const Baz & baz) {
   func_1(baz.a);
   some_function(__PRETTY_FUNCTION__);
 }
};

void func() {
 Bar bar;
 Zonk *rep;
 rep->func_1(bar);
 rep->func_2(Baz(bar));
}

void foo ()
{
  func();
}
