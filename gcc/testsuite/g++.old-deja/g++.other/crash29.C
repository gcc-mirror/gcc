// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

// crash test - XFAIL *-*-*

class bar
{
public:
  void foo ();
  void baz ();
};
void bar::foo ()
{
  baz x();			// ERROR - 
}
