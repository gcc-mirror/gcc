// PR c++/17530
// { dg-do link }

typedef void (*Func) ();
void f (Func) {}
struct B
{
  static void staticfunc () {}
};
template <int> 
void C(){ f (B::staticfunc); }
int main ()
{
  C<0>();
  return 0;
}
