// PR c++/56530
// { dg-options "-Wsign-conversion" }

struct string
{
  string () {};
  ~string () {};
};

string foo[1];      // okay
string bar[1][1];   // gives bogus warning
