// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// crash test - XFAIL *-*-*
// excess errors test - XFAIL *-*-*

namespace bar
{
struct foo
{
  foo();
};			// ERROR - parse error XFAIL *-*-*
