// PR c++/82040
// { dg-do compile { target c++11 } }
// { dg-options "-Wbool-operation" }

template <class c>
decltype (~c{})
call ()
{
  return ~false; // { dg-warning "on an expression of type bool" }
}
template int call<bool>();
