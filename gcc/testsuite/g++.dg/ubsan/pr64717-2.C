// PR c++/64717
// { dg-do compile }
// { dg-options "-O2 -Wuninitialized -fsanitize=vptr" }

class ios {};

struct stringstream : virtual ios {
  stringstream (char *);
  ~stringstream ();
};

struct string { char *c_str (); };

string make_str ();

void
bar ()
{
  stringstream param (make_str ().c_str ()); // { dg-bogus "is used uninitialized" }
}
