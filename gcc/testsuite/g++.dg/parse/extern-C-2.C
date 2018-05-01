// PR c++/85580

extern "C"
{

  void f1()
  {
    union some_type{
      char a[2];
      int b;
    } variable;
  }

  void f2()
  {
    union some_type{
      char a[2];
      int b;
    } variable;
  }

}
