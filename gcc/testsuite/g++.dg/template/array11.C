// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// PR c++/19208: Fold dependent array domains

template <class C> struct if_t { typedef int type; };
template <class T> struct hhhh { static const bool value = true; };
template <class T> struct gggg { static const bool value = hhhh<T>::value; };
template <class T> struct ffff { static const bool value = gggg<T>::value; };
template <class A>
struct bound_member_action
{
  typedef char f[ffff<A>::value ? 1 : 2];
  template <class CT>
  bound_member_action(CT i, typename if_t<f>::type g)  {}
};

bound_member_action<int> a(0, 1);
