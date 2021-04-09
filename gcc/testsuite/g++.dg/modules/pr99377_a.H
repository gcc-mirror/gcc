// PR 99377 failed to stream locally instantiated member
// link failure in function `Check(Widget<int> const&)':
// bug_c.ii:(.text._Z5CheckRK6WidgetIiE[_Z5CheckRK6WidgetIiE]+0x14): undefined reference to `Widget<int>::Second() const'
// { dg-module-do link }
// { dg-additional-options -fmodule-header }
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

