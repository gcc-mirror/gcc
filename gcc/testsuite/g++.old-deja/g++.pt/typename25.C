// Build don't link:
// Special g++ Options: 
// Copyright (C) 2000 Free Software Foundation
// Adapted by Nathan Sidwell 1 July 2000 <nathan@codesourcery.com>
// Derived from a bug report by scott snyder <snyder@fnal.gov>
// Our implicit typename extension was causing this pedantically
// correct program to fail

struct list
{
  typedef int reference;
};

class d0_Collection_Base {};


template <class T>
class d0_List_1
  : virtual public d0_Collection_Base,
    public list
{
public:
  typedef int reference;
};

template <class T>
class d0_List
{
public:
  typedef d0_List_1<T> Base;
  typedef typename Base::reference              reference;
};
