// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attribute noreturn from a primary template declared with
// one.
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized" }

struct Noreturn { };
struct Returns { };

// Primary declared noreturn but explicit specialization is not.
template <class T> int __attribute__ ((noreturn)) f ();
template <>        int                            f<Returns>();

// Explicit specialization is noreturn but primary is not.
template <class T> int g ();
template <>        int __attribute__ ((noreturn)) g<Noreturn>();

int val;

int test_primary_noreturn (char, short)
{
  // Only the first call should be emitted, the second one should
  // be eliminated because the first one doesn't return.
  val = f<char>() + f<short>();
}   // expect no -Wreturn-type warning here

int test_noreturn (int)
{
  // Call should be retained.
  f<int>();
}   // expect no -Wreturn-type warning here

int test_special_return (int)
{
  // Both calls must be emitted.
  int val = f<Returns>() + f<Returns>();
  (void)&val;
}   // { dg-warning "no return statement in function returning non-void" }


int test_primary_return (void)
{
  int val = g<char>() + g<int>();
  (void)&val;
}   // { dg-warning "no return statement in function returning non-void" }


int test_special_noreturn (int, long)
{
  g<Noreturn>();
}   // expect no -Wreturn-type warning here


// Verify that the call to f<short>() above is eliminated but the call
// to f<int>() and the two calls to f<Returns>() are retained.
// { dg-final { scan-tree-dump-not "f<short>" "optimized" } }
// { dg-final { scan-tree-dump-times "f<Returns>" 2 "optimized" } }

// Verify that the second call to f<Returns>() in test_special_return()
// is followed by __builtin_unreachable() because there is no return
// statement in the function.
// { dg-final { scan-tree-dump-times "f<Returns> \\(\\);\[\n\r \]+__builtin_unreachable" 1 "optimized" } }


// Verify that the call to g<short>() above is eliminated but the call
// to g<char>() and to g<Noreturn>() are both retained.
// { dg-final { scan-tree-dump-not "g<short>" "optimized" } }
// { dg-final { scan-tree-dump-times "g<char>" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "g<Noreturn>" 1 "optimized" } }

// Verify that the call to g<int>() in test_primary_return() is
// followed by __builtin_unreachable() because there is no return
// statement in the function.
// { dg-final { scan-tree-dump-times "g<int> *\\(\\);\[\n\r \]+__builtin_unreachable" 1 "optimized" } }
// Verify that the call to g<Noreturn>() in test_special_noreturn()
// is not followed by __builtin_unreachable() even though there is no
// return statement in the function.
// { dg-final { scan-tree-dump-times "g<Noreturn> *\\(\\);\[\n\r \]+__builtin_unreachable" 0 "optimized" } }
