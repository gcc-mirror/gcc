// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Aug 2003 <nathan@codesourcery.com>

// PR 11766. ICE

template<typename T>
struct normal_iterator
{
  normal_iterator(const T& __i);
};


template<typename _Tp>
struct vector
{
  void end() const {  normal_iterator<const _Tp*> (this->pt); }
  void size() const { end(); }
  _Tp* pt;
};
  


struct MuonTag {
  typedef void (MuonTag::*Selector)();
};

void foo()
{
  vector<MuonTag::Selector> _selectors;
  _selectors.size();
}
