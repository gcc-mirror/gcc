// { dg-do compile }
// Origin: <v dot haisman at sh dot cvut dot cz>
// PR c++/13957: Improved error message for type in template (when non-type
//  is expected).

template <class T>
struct A
{
  typedef int type;
};

template <class T>
void func(void)
{
  (void)A<T>::type();	 // { dg-error "if a type is meant" }
// { dg-error "parsed as a non-type" "non-type" { target *-*-* } 15 }
}

template void func<float>(void);    // { dg-error "instantiated from here" }
