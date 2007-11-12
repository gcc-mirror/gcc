// PR c++/29225
// { dg-do compile }

template <typename L, typename R> bool operator< (L x, R y);
struct T { int t (); };
class S {};

struct U
{
   typedef int (T::* M) ();
   M m;

   bool operator() (S &x)
   {
     T a;
     return (a.*m) < x;	// { dg-error "invalid use of non-static member" }
   }
};

void foo (S &x)
{
  U m;
  m.m = &T::t;
  m (x);
}
