// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Dec 2001 <nathan@codesourcery.com>

// PR 5116 Failed to find friend in overload resolution

int wrong;
int right;

struct Printer 
{
  Printer &operator<< (bool a)
  {
    wrong++;
    
    return *this;
  }
  
};

struct Buggy {};

template <typename T> struct Handle
{
  Handle(T* p) {}
  
  operator bool() const { return true; }
  
  friend Printer& operator<<(Printer& ostr, const Handle& r)
  {
    right++;

    return ostr;
    
  }
};

typedef Handle<Buggy>     Buggy_h;

Printer out;

bool cmp (const Buggy_h& b1, const Buggy_h& b2)
{
  out << b1 << b2;
  return false;
}

int main()
{
  Buggy o;
  
  cmp (&o, &o);

  if (wrong)
    return 1;
  if (right != 2)
    return 2;
  return 0;
}
