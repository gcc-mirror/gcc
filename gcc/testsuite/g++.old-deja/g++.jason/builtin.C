// Bug: g++ overloads strlen instead of bashing the builtin version.
// Special g++ Options: -w
// Build don't link:

extern "C" void strlen (const char *);

void f ()
{
  strlen("Hi");
}
