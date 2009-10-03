// { dg-do compile }
// { dg-options "-O1" }
struct Foo { };
struct Bar { virtual ~Bar(); };
struct Baz: public virtual Bar { virtual void Func (Foo); };
void unused() { Baz().Func(Foo()); }
