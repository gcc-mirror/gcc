// { dg-do assemble  }

class A
{
  template<class T>T epsilon; // { dg-error "" } invalid member template
};
