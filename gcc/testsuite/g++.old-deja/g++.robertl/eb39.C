// Build don't link: 
#include <cctype>
#include <iostream>
#include <sstream>
#include <cstring>

using namespace std;

extern bool foo2 (ostream &out, istream &in);

bool
foo1 (ostream &out, const char *in)
{
  string tmp(in, std::strlen(in));
  stringbuf sb (tmp);
  istream fmt (&sb);
  return foo2 (out, fmt);
}
