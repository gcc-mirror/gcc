// { dg-do assemble  }
// { dg-options "-w" }
// Bug: g++ overloads strlen instead of bashing the builtin version.

extern "C" void strlen (const char *);

void f ()
{
  strlen("Hi");
}
