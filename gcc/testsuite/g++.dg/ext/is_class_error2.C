// PR c++/33464

template<int> void foo()
{
  __has_nothrow_assign(int)(); // { dg-error "'__has_nothrow_assign\\(int\\)' cannot be used" }
  __has_trivial_assign(int)(); // { dg-error "'__has_trivial_assign\\(int\\)' cannot be used" }
  __has_nothrow_constructor(int)(); // { dg-error "'__has_nothrow_constructor\\(int\\)' cannot be used" }
  __has_trivial_constructor(int)(); // { dg-error "'__has_trivial_constructor\\(int\\)' cannot be used" } 
  __has_nothrow_copy(int)(); // { dg-error "'__has_nothrow_copy\\(int\\)' cannot be used" }
  __has_trivial_copy(int)(); // { dg-error "'__has_trivial_copy\\(int\\)' cannot be used" }
  __has_trivial_destructor(int)(); // { dg-error "'__has_trivial_destructor\\(int\\)' cannot be used" }
  __has_virtual_destructor(int)(); // { dg-error "'__has_virtual_destructor\\(int\\)' cannot be used" }
  __is_abstract(int)(); // { dg-error "'__is_abstract\\(int\\)' cannot be used" }
  __is_base_of(int, float)(); // { dg-error "'__is_base_of\\(int, float\\)' cannot be used" }
  __is_class(int)(); // { dg-error "'__is_class\\(int\\)' cannot be used" }
  __is_empty(int)(); // { dg-error "'__is_empty\\(int\\)' cannot be used" }
  __is_enum(int)(); // { dg-error "'__is_enum\\(int\\)' cannot be used" }
  __is_pod(int)(); // { dg-error "'__is_pod\\(int\\)' cannot be used" }
  __is_polymorphic(int)(); // { dg-error "'__is_polymorphic\\(int\\)' cannot be used" }
  __is_union(int)(); // { dg-error "'__is_union\\(int\\)' cannot be used" }
}
