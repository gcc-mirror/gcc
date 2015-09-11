// PR c++/65811

struct foo { int i; };

static void fn1 ();
inline void
fn1 ()
{
  static struct foo a[1];
}
