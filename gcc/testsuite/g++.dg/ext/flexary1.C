// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR c++ 11614

typedef int ary_t[];

struct test
{
  ary_t *b;
  int (*a)[]; // this is not a flexible array member
};
 
void test(void)
{
  struct test s;
  int (*a)[] = 0;
  ary_t *b = 0;
  
  a = s.a;
  a = s.b;

  s.a = a;
  s.b = a;

  b = s.a;
  b = s.b;

  s.a = b;
  s.b = b;
}
