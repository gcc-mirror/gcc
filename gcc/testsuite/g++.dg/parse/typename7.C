// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de> and
// Alexandre Oliva <aoliva@redhat.com>

// PR c++/18757: ICE in get_innermost_template_args

struct A
{
  template<typename>   void foo(int);
  template<typename T> void bar(T t) {
    this->foo<typename T>(t); } // { dg-error "expected|parse error|no matching" }
  template<typename T> void bad(T t) {
    foo<typename T>(t); } // { dg-error "expected|parse error" }
};

template <typename T>
struct B
{
  void bar(T t) {
    A().bar<typename T>(t); } // { dg-error "expected|parse error|no matching" }
  void bad(T t) {
    B<typename T>::bar(t); } // { dg-error "invalid|not a template" }
};

void baz()
{
  A().bar(0);
  A().bad(0);
  B<int>().bar(0);
}
