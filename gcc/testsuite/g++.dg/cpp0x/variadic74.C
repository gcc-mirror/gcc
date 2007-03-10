// { dg-options "-std=gnu++0x" }
template <class... Types> class A
{
public:
  template <Types... Values> class X { /* ... */ }; // { dg-error "not a valid type for a template constant parameter" }
};

template<class... Types> class B
{
public:
  template <Types*... Values> class X { 
    typename A<Types*...>::template X<Values...> foo;
  };
};

int i;
float f;

A<int*, float*>::X<&i, &f> apple1;
B<int, float>::X<&i, &f> banana1;

A<int*, float*>::X<&i> apple2; // { dg-error "wrong number of template arguments" }
// { dg-error "invalid type" "" { target *-*-* } 22 }
A<int*, float*>::X<&i, &f, &f> apple3; // { dg-error "wrong number of template arguments" }
// { dg-error "invalid type" "" { target *-*-* } 24 }
A<int, float> apple4;
