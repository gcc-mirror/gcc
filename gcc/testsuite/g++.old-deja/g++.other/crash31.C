// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// excess errors test - XFAIL *-*-*

namespace bar
{
struct foo
{
  foo();
};
			// ERROR - parse error XFAIL *-*-*
