// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

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
