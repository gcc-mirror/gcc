// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Apr 2005 <nathan@codesourcery.com>

// PR 20723
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>
//         Nathan Sidwell <nathan@gcc.gnu.org>

template <typename T> class srp;
template <typename T> struct ptr
{
  template <typename U> ptr(const srp<U> &other); // { dg-message "ptr<T>::ptr" }
};
template <typename T> struct srp
{
  template <typename U> operator ptr<U>(void) const; // { dg-message "srp<T>::operator" }
};
ptr<int> parent_get()
{
  srp<int> parent;		// { dg-message "candidate" }
  return parent; // { dg-error "is ambiguous" }
}
