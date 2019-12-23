// PR c++/17042
// { dg-do assemble }
/* { dg-require-weak "" } */
// { dg-require-effective-target cxa_atexit }
// { dg-options "-fuse-cxa-atexit" }

struct A
{  A();  ~A(); };
A a;
extern "C" { void* __dso_handle __attribute__ ((__weak__)); }
void f()
{  __dso_handle = 0; }
