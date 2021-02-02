// { dg-additional-options -fmodules-ts }

export module Foo;
// { dg-module-cmi Foo }

export class Bit
{
private:
  unsigned _M_msb:1;
};

Bit Make () noexcept;

export class Container
{
public:
  void Frob ()
  {
    _M_rep = Make ();
  }
  
private:
  Bit _M_rep;
};
