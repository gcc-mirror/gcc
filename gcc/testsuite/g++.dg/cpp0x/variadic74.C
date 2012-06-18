// { dg-do compile { target c++11 } }
template <class... Types> class A
{
public:
  template <Types... Values> class X { /* ... */ }; // { dg-error "not a valid type for a template non-type parameter" }
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

A<int*, float*>::X<&i> apple2; // { dg-error "wrong number of template arguments" "wrong number" }
// { dg-error "invalid type" "invalid" { target *-*-* } 22 }
A<int*, float*>::X<&i, &f, &f> apple3; // { dg-error "wrong number of template arguments" "wrong number" }
// { dg-error "invalid type" "invalid" { target *-*-* } 24 }
A<int, float> apple4;
