// PR target/27650
// Don't use dllimport semantics on virtual methods
// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }

// Don't import explicitly virtual method.
struct base
{
  virtual void key_method();
  __attribute__((dllimport)) virtual ~base();
};

void base::key_method() {}


// Nor an implicitly virtual method.
struct derived : public base
{
  void key_method(); 
  __attribute__((dllimport)) ~derived();
};

void derived::key_method() {}
