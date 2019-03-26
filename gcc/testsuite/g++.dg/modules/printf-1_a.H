// { dg-module-do run }
// { dg-additional-options -fmodule-header }
// { dg-module-bmi {} }

extern "C" 
{
  // this matches an anticipated fn
  extern int printf (const char *__restrict, ...);
}
