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
  (void)A<T>::type();	 // { dg-error "non-type" "non-type" }
// { dg-message "if a type" "note" { target *-*-* } .-1 }
}

template void func<float>(void);    // { dg-message "required from here" }
