// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// crash test - XFAIL *-*-*

void foo()
{
  if [		// ERROR - parse error
}
