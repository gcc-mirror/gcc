// Build don't link: 
#include <ctype.h>
#include <iostream.h>
// #include <streambuf.h>
#include <libio.h>
#include <strstream.h>

extern bool foo2 (ostream &out, istream &in);

bool
foo1 (ostream &out, const char *in)
{
  strstreambuf sb (in, (int) strlen (in));
  istream fmt (&sb);
  return foo2 (out, fmt);
}
