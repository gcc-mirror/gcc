// ICE with overloads not ordering using decls.  Failed to invoke
// deduping logic

void remove (const char *);

namespace std
{
  using ::remove;

  void remove ();
}

using namespace std;

void test01 ()
{
  remove (0);
}
