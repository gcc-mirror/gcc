// Special g++ Options: -O2
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 May 2001 <nathan@codesourcery.com>

// Bug 2936. We ICE'd on tree inlining a function with an anonymous
// union decl.

inline const unsigned char *Foo (const char *string)
{
  union
  {
    const char *p1;
    const unsigned char *p2;
  };
  p1 = 0;
  p2 = 0;


  p1 = string;
  return p2;
  
}

const unsigned char *Baz (const char *string)
{
  return Foo (string);
}

int main ()
{
  const char *string = "s";
  const unsigned char *result;

  result = Baz (string);
  return (static_cast <const void *> (result)
	  != static_cast <const void *> (string));
}
