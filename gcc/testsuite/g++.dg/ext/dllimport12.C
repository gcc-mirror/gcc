// PR target/27650
// Don't use dllimport semantics on virtual methods when initializing
// vtables
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

// Use import lib thunk for vtable entry of explicitly virtual method,
struct base
{
  virtual void key_method();
  __attribute__((dllimport)) virtual ~base();
};

void base::key_method() {}


// Likewise for an implicitly virtual method.
struct derived : public base
{
  void key_method(); 
  __attribute__((dllimport)) ~derived();
};

void derived::key_method() {}
