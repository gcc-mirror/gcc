// PR c++/29234

struct S { void operator()(); };

void foo ()
{
  ( S()() );
}

struct C { void operator[](C); };

void bar ()
{
  C x;
  ( C()[x] );
}
