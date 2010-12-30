// PR c++/46001
// { dg-do compile }

struct S
{
  char *p;
  unsigned char f : 1;
};

struct S s;
void *a = s.p | s.f;	// { dg-error "unsigned char:1" }

// { dg-bogus "__java_boolean" "" { target *-*-* } 11 }
