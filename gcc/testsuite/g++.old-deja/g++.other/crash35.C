// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

void foo()
{
  if [		// ERROR - parse error
}
