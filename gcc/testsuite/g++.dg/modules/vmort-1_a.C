// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module tom.riddle;
// { dg-module-bmi tom.riddle }

export inline auto One (int a)
{
  struct X {
    int x;
    X(int a) :x(a){}
    operator int () const {return x;}
  };

  return X(a);
}

// Look Ma! this isn't inline!
export auto Two (int a)
{
  struct Y {
    int x;
    Y(int a) :x(a){}
    operator int () const {return x;}
  };

  return Y(a);
}

