// { dg-options "-Wcatch-value=3" }

struct A { virtual ~A() {} };
struct B : A {};
struct C {};
struct D : C {};

void foo()
{
  try {}
  catch (D)    {}  // { dg-warning "catching type" }
  catch (C)    {}  // { dg-warning "catching type" }
  catch (B)    {}  // { dg-warning "catching polymorphic type" }
  catch (A)    {}  // { dg-warning "catching polymorphic type" }
  catch (A*)   {}  // { dg-warning "catching non-reference type" }
  catch (int)  {}  // { dg-warning "catching non-reference type" }

  try {}
  catch (D&)   {}
  catch (C&)   {}
  catch (B&)   {}
  catch (A&)   {}
  catch (A*)   {}  // { dg-warning "catching non-reference type" }
  catch (int&) {}
}

template<typename T> void foo1()
{
  try {}
  catch (T) {}  // { dg-warning "catching" }
}

template<typename T> void foo2()
{
  try {}
  catch (T*) {}  // { dg-warning "catching non-reference type" }

  try {}
  catch (T&) {}

  try {}
  catch (const T&) {}
}

void bar()
{
  foo1<int&>();
  foo1<const A&>();
  foo1<B&>();
  foo1<const C&>();
  foo1<D&>();

  foo1<int>();  // { dg-message "required" }
  foo1<A>();    // { dg-message "required" }
  foo1<B>();    // { dg-message "required" }
  foo1<C>();    // { dg-message "required" }
  foo1<D>();    // { dg-message "required" }

  foo2<int>();  // { dg-message "required" }
  foo2<A>();    // { dg-message "required" }
  foo2<B>();    // { dg-message "required" }
  foo2<C>();    // { dg-message "required" }
  foo2<D>();    // { dg-message "required" }
}
