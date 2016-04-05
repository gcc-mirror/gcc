// PR c++/70512

struct S 
{
  S& operator= (int)
  {
    return *this;
  }
} __attribute__ ((__may_alias__));
