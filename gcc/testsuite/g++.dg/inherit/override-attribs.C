// PR c++/14688
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }

class one
{
public:
  virtual void
  test(void* value);  // { dg-error "overriding" }
};

class two : public one
{
public:
  void  __attribute__((regparm(2)))
  test(void* value);  // { dg-error "conflicting type attributes"  }
};

class three : public one
{
public:
  void __attribute__ ((cdecl))
  test(void* value);  // OK
};
