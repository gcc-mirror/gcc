// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Apr 1999 <nathan@acm.org>

struct X
{
  int i;
  
  X():i(){}
  void *operator new(unsigned)
  {
    return 0; // WARNING - cannot return NULL
  }
  void *operator new[](unsigned)
  {
    return 0; // WARNING - cannot return NULL
  }
};

struct Y
{
  int i;
  
  Y():i(){}
  void *operator new(unsigned) throw()
  {
    return 0; // ok
  }
  void *operator new[](unsigned) throw()
  {
    return 0; // ok
  }
};

int main()
{
  Y *yp = new Y;
  
  return yp != 0;
}
