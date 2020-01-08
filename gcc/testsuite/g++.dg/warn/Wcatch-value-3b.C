// { dg-options "-Wcatch-value=3" }

struct A { virtual ~A() {} };
struct B : A {};
struct C {};
struct D : C {};

void foo()
{
  try {}
  catch (D d)    {}  // { dg-warning "12:catching type" }
  catch (C c)    {}  // { dg-warning "12:catching type" }
  catch (B b)    {}  // { dg-warning "12:catching polymorphic type" }
  catch (A a)    {}  // { dg-warning "12:catching polymorphic type" }
  catch (A* a)   {}  // { dg-warning "13:catching non-reference type" }
  catch (int i)  {}  // { dg-warning "14:catching non-reference type" }

  try {}
  catch (D& d)   {}
  catch (C& c)   {}
  catch (B& b)   {}
  catch (A& a)   {}
  catch (A* a)   {}  // { dg-warning "13:catching non-reference type" }
  catch (int& i) {}
}

template<typename T> void foo1()
{
  try {}
  catch (T t) {}  // { dg-warning "12:catching" }
}

template<typename T> void foo2()
{
  try {}
  catch (T* t) {}  // { dg-warning "13:catching non-reference type" }

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
