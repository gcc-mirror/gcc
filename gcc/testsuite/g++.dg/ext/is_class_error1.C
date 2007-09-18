// PR c++/33212

template<int> void foo()
{
  __is_class((int); // { dg-error "type-specifier|primary-expression" }
}
