// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

void foo()
{
  if [		// { dg-error "" } parse error
}
