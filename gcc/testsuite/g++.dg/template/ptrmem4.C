// { dg-do compile }

// Origin: Scott Snyder <snyder@fnal.gov>

// PR c++/8849
// Pointer to member function template argument deduction ICE.


template <class CONT> void queryAliases(CONT& fill_me);

struct SpyExample
{
  void ready();
  void inputs();
};

void SpyExample::ready()
{
  queryAliases(inputs); // { dg-error "invalid" }
}
