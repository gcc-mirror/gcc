// PR c++/72759
// { dg-do compile { target c++14 } }

template <typename> struct SpecPerType;
class Specializer {
  public:  template <bool> static void MbrFnTempl();
  template <unsigned> struct A { static void InnerMemberFn(); };
  void Trigger() { A<0>::InnerMemberFn; }
};
template <> struct SpecPerType<Specializer> {
  using FnType = void *;
  template <bool P>
  static constexpr FnType SpecMbrFnPtr = Specializer::MbrFnTempl<P>;
};
template <unsigned X> void Specializer::A<X>::InnerMemberFn() {
  using Spec = SpecPerType<Specializer>;
  Spec ErrorSite = Spec::SpecMbrFnPtr<SpecMbrFnPtr>;  // { dg-error "not declared" }
}
