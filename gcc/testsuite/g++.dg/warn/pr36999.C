/* PR36999: Erroneous "does not declare anything" warnings.  */
/* { dg-do compile } */

class C1 {
 public: class C2 { };
};

void cf1 (class C1::C2, void*);  // { dg-bogus "does not declare anything" }
void cf2 (void*, class C1::C2);
void cf3 (C1::C2, void*);

namespace N {

enum E1 { foo }; 
enum E2 { bar }; 

template <class X>
class TC1 { };

template <class T, class U>
class TC2 : public TC1<T> { };

}

void
tcf1 (N::TC2<enum N::E1, void*> *arg1,  // { dg-bogus "does not declare anything" }
      N::TC2<void*, enum N::E1> *arg2,
      N::TC2<N::E1, void*> *arg3)
{
}

void *
tcf2 (void *x)
{
  return (void *)
    (N::TC2<enum N::E1, void*> *)  // { dg-bogus "does not declare anything" }
    (N::TC2<void*, enum N::E1> *)
    (N::TC2<N::E1, void*> *)
    x;
}
