// PR c++/51908
// { dg-do compile { target c++11 } }

struct foo1
{
  template <typename Ret, typename... Args>
  operator decltype(static_cast<Ret (*)(Args...)>(nullptr)) () const;
};

struct foo2
{
  template <typename Ret, typename... Args>
  operator decltype(static_cast<Ret (*)(Args... args)>(nullptr)) () const;
};

struct foo3
{
  template <typename Ret, typename Arg>
  operator decltype(static_cast<Ret (*)(Arg)>(nullptr)) () const;
};

struct foo4
{
  template <typename Ret, typename Arg>
  operator decltype(static_cast<Ret (*)(Arg arg)>(nullptr)) () const;
};
