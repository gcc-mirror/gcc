// PR c++/79548 - missing -Wunused-variable on a typedef'd variable
// in a function template
// { dg-do compile }
// { dg-options "-Wunused" }


#define UNUSED __attribute__ ((unused))

template <class T>
void f_int ()
{
  T t;                        // { dg-warning "unused variable" }

  typedef T U;
  U u;                        // { dg-warning "unused variable" }
}

template void f_int<int>();


template <class T>
void f_intptr ()
{
  T *t = 0;                   // { dg-warning "unused variable" }

  typedef T U;
  U *u = 0;                   // { dg-warning "unused variable" }
}

template void f_intptr<int>();


template <class T>
void f_var_unused ()
{
  // The variable is marked unused.
  T t UNUSED;

  typedef T U;
  U u UNUSED;
}

template void f_var_unused<int>();


template <class T>
void f_var_type_unused ()
{
  // The variable's type is marked unused.
  T* UNUSED t = new T;   // { dg-bogus "unused variable" "bug 79585" }

  typedef T U;
  U* UNUSED u = new U;   // { dg-bogus "unused variable" "bug 79585" }

  typedef T UNUSED U;
  U v = U ();   // { dg-bogus "unused variable" "bug 79585" }
}

template void f_var_type_unused<int>();


struct A { int i; };

template <class T>
void f_A ()
{
  T t;                        // { dg-warning "unused variable" }

  typedef T U;
  U u;                        // { dg-warning "unused variable" }
}

template void f_A<A>();


template <class T>
void f_A_unused ()
{
  T t UNUSED;

  typedef T U;
  U u UNUSED;
}

template void f_A_unused<A>();


struct B { B (); };

template <class T>
void f_B ()
{
  T t;

  typedef T U;
  U u;
}

template void f_B<B>();


struct NonTrivialDtor { ~NonTrivialDtor (); };

template <class T>
void f_with_NonTrivialDtor ()
{
  // Expect no warnings when instantiated on a type with a non-trivial
  // destructor.
  T t;

  typedef T U;
  U u;
}

template void f_with_NonTrivialDtor<NonTrivialDtor>();


struct D { NonTrivialDtor b; };

template <class T>
void f_D ()
{
  // Same as f_with_NonTrivialDtor but with a class that has a member
  // with a non-trivial dtor.
  T t;

  typedef T U;
  U u;
}

template void f_D<D>();


struct UNUSED DeclaredUnused { };

template <class T>
void f_with_unused ()
{
  // Expect no warnings when instantiatiated on a type declared
  // with attribute unused.
  T t;

  typedef T U;
  U u;
}

template void f_with_unused<DeclaredUnused>();
