/* PR debug/5770
   This testcase failed at -O -g because the following constants
   were optimized away since they were never referenced, but
   since they are variables with initializers, rtl_for_decl_location
   run expand_expr on their initializers and returned it.
   This lead to references to constants which were deferred and thus
   never emitted.  */
/* { dg-do link } */

static const char foo[] = "foo string";
static const char bar[30] = "bar string";
static const wchar_t baz[] = L"baz string";

int
main ()
{
}
