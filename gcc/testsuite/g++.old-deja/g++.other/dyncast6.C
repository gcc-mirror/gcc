// Special g++ Options: -w -ansi -pedantic-errors

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 143 from Carlo Wood <carlo@runaway.xs4all.nl>
// We were generating incorrect type_info structures, and hence breaking
// dynamic_cast.

#include <stdio.h>

class OBASE { public: virtual void bz () {}};
class IBASE { public: virtual void by () {}};

class V {public:int m; };

class A : public virtual V { };
class AA : public A {};

class B : public OBASE, public A { public: virtual void foo(void) { } };
class B1 : public OBASE, public AA { public: virtual void foo(void) { } };

class C : public IBASE, public virtual V { };

class D : public B, public C { };

class E : public B, public virtual V { };

class E1 : public B1, public virtual V {};

class E2 : public B1, public A, public virtual V {};

int main(void)
{
  D d;
  E e;
  E1 e1;
  E2 e2;
  int code = 0;

  OBASE* osd = &d;
  OBASE* ose = &e;
  OBASE* ose1 = &e1;
  OBASE* ose2 = &e2;


  if (!dynamic_cast<V*>(osd))
    {
      printf ("fail osd\n");
      code++;
    }
  
  if (!dynamic_cast<V*>(ose))
    {
      printf ("fail ose\n");
      code++;
    }

  if (!dynamic_cast<V*>(ose1))
    {
      printf ("fail ose1\n");
      code++;
    }

  if (!dynamic_cast<V*>(ose2))
    {
      printf ("fail ose2\n");
      code++;
    }

  return code;
}
