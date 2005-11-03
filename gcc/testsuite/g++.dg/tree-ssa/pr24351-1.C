/* { dg-do compile } */
/* { dg-options "-O2" } */
struct adaptor_base {
};
struct bound_argument {
  bound_argument();
};
template <class T_functor> struct adaptor_functor : public adaptor_base {
  explicit adaptor_functor(const T_functor& _A_functor) : functor_(_A_functor)
{
  }
  T_functor functor_;
  bound_argument bound_;
};
template <class T_functor> struct adapts : public adaptor_base {
  explicit adapts(const T_functor& _A_functor) : functor_(_A_functor) {
  }
  adaptor_functor<T_functor> functor_;
};
int main() {
  adapts<adapts<int> > a (adapts<int>(1));
}


