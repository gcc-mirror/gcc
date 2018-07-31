/* PR c++/86329: ensure we don't erroneously offer suggestions like "._0",
   which are an implementation detail of how e.g. anonymous enums are
   handled internally.  */
   
enum {NONEMPTY};

int test()
{
  return __0; // { dg-error "'__0' was not declared in this scope" }
  // { dg-bogus "suggested alternative" "" { target *-*-* } .-1 }
}
