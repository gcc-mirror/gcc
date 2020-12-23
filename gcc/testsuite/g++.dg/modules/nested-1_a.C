// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module blinky;
// { dg-module-cmi blinky }

export struct X 
{
  struct Inner 
  {
    int m;
    Inner (int);
    int getter () const
    {
      return m;
    }
  };
};

