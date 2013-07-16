// PR c++/40405

template<int, int> struct A
{
  static int i;
};

template<int> int A<0,0>::i = 0; // { dg-error "" }

int j = A<0,0>::i;
