// PR c++/32019

struct C
{
  C(const char *);
  operator const char *();
};

extern C c;
extern const char * s;

void
foo (bool b)
{
  b ? c : s;			// { dg-error "?:" }
  // { dg-message "convert" "" { target *-*-* } .-1 }
}
