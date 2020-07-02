// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module tom.riddle;
// { dg-module-cmi tom.riddle }

export inline auto One (int a)
{
  struct X {
    int x;
    // p1779 makes these things not-inline, which is a surprise.
    // Asking CWG
    inline X(int a) :x(a){}
    inline operator int () const {return x;}
  };

  return X(a);
}

// Look Ma! this isn't inline!
export auto Two (int a)
{
  struct Y {
    int x;
    // In this case we do manage to emit these fns (if not marked as
    // inline), but we give them internal linkage, so they are not
    // nameable from elsewhere.  Workaround for now.
    inline Y(int a) :x(a){}
    inline operator int () const {return x;}
  };

  return Y(a);
}

