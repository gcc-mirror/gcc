// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// crash test - XFAIL *-*-*

namespace bar
{
struct foo
{
  foo();
};			// ERROR - parse error
