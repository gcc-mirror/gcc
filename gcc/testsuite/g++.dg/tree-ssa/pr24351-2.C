/* { dg-do compile } */
/* { dg-options "-O2" } */
struct adaptor_base {};
struct bound_argument {
  bound_argument();
};
struct adaptor_functorint : public adaptor_base {};
struct adaptsint : public adaptor_base {
  adaptsint(const int& _A_functor);
  adaptor_functorint functor_;
};
struct adaptor_functor_adaptsint {
  adaptor_functor_adaptsint(const adaptsint& _A_functor) : functor_(_A_functor)
  {}
  adaptsint functor_;
  bound_argument bound_;
};
struct adapts_adaptsint {
  adapts_adaptsint(const adaptsint& _A_functor) : functor_(_A_functor)
  {}
  adaptor_functor_adaptsint functor_;
};
int main() {
  adapts_adaptsint a (adaptsint(1));
}
