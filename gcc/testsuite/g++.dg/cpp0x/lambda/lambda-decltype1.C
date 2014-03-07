// PR c++/59791
// We force the gimple dump to trigger use of lang_decl_name.
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-gimple" }
// { dg-final { cleanup-tree-dump "gimple" } }

template < class T > void
f (T t)
{
  int i = t;
  [](int)->decltype (i + t)
  {
    return 0;
  }
  (0);
}

void
foo ()
{
  f (0);
}
