// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 May 1999 <nathan@acm.org>

// Although anon unions cannot have user defined member functions
// [class.union/2].  They should have implicitly defined copy ctors and
// and the like [class.copy/4].  Make sure we generate one of the correct
// signature and that it works ok.

extern "C" void abort();

struct A
{
  union
  {
    int a;
  };
};
union B
{
  int a;
};

static A Ctor(A const &src)
{
  A result(src);  // this should not cause a const violation
  
  result = src;   // and neither should this
  
  return result;
}

typedef __SIZE_TYPE__ size_t;

void *operator new(size_t, void *ptr)
{
  return ptr;
}

// check copy ctor and assignment for plain union
void check_union()
{
  B b1;
  B b2;
  
  b1.a = 5;
  b2.a = 6;
  b2 = b1;
  if(b2.a != 5)
    abort();
  
  b2.a = 6;
  new (&b2) B(b1);
  if(b2.a != 5)
    abort();
  
  return;
}

// check copy ctor and assignment for class containing anon-union
void check_union_member()
{
  A a1;
  A a2;
  
  a1.a = 5;
  a2.a = 6;
  a2 = a1;
  if(a2.a != 5)
    abort();
  
  a2.a = 6;
  new (&a2) A(a1);
  if(a2.a != 5)
    abort();
  
  return;
}

int main()
{
  check_union();
  check_union_member();
  
  return 0;
}
