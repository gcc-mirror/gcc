// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

export struct timex
{
  int a;
  int :32; 
  int :32; 
  int :32; 
  int :32; 
  int :32; 
  int :32; 
};

