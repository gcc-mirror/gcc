// { dg-do assemble  }

class A
{
  template<class T>T epsilon; // { dg-error "22:data member .epsilon. cannot be a member template" }
};
