// { dg-module-do run }
export module blinky;
// { dg-module-bmi blinky }

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

