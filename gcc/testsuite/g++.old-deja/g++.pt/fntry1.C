// Bug: g++ silently ignores function-try-blocks in templates.
// Submitted by Jason Merrill <jason@cygnus.com>

template <class T> void f (T) try { throw 1; } catch (...) { }

int main ()
{
  f (1);
}
