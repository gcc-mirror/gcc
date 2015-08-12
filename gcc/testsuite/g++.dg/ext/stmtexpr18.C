// PR c++/66445
// { dg-options "" }

template <typename> void foo ()
{
  int a = ({ struct A{} b; 42; });
}
