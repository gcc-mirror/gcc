// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module frob;
// { dg-module-cmi "frob" }

export struct A
{
  operator int () 
  {
    return 0;
  }
};
