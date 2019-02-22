// { dg-module-do run }
// { dg-additional-options -fmodule-header }
// { dg-module-bmi {"printf-1_a.H"} }

extern "C" 
{
  // this matches an anticipated fn
  extern int printf (const char *__restrict, ...);
}
