// { dg-do assemble }
// Origin: Jakub Jelinek <jakub@redhat.com>

// { dg-bogus "" "" { xfail *-*-* } 13 }

namespace bar
{
struct foo
{
  foo();
};
			// { dg-error "" "" { xfail *-*-* } }
