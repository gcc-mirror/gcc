// { dg-additional-options -fmodules-ts }
export module baz;
// { dg-module-cmi baz }

import foo;

export struct Container  : virtual Derived
{
  Container () {}
  ~Container () {}
};
  
  
  
