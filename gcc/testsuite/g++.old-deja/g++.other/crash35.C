// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// excess errors test - XFAIL *-*-*

void foo()
{
  if [		// ERROR - parse error
}
