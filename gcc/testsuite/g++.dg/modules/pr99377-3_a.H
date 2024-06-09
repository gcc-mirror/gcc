// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template<typename>
struct Widget
{
  Widget (int) { }

  bool First() const { return true; }

  bool Second () const { return true;}
};

inline void Frob (const Widget<int>& w) noexcept
{
  w.First ();
}
