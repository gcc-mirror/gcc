// { dg-module-do run }
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

extern "C" 
{
  // this matches an anticipated fn
  extern int printf (const char *__restrict, ...);
}
