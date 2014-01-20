// PR middle-end/59706
// { dg-do compile }

extern struct S s;
struct T { T (); ~T (); int t; } t;

void
foo ()
{
  #pragma GCC ivdep
  while (s)	// { dg-error "could not convert" }
    ;
}

void
bar ()
{
  #pragma GCC ivdep
  while (t)	// { dg-error "could not convert" }
    ;
}
