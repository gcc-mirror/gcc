// { dg-do assemble  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

union A
{
  int i;
  int j;

  A () : i (3), j (2) {} // { dg-error "" } multiple initializations
};

union B
{
  int i;
  union {
    int j;
  };

  B () : i (3), j (2) {} // { dg-error "" } multiple initializations
};

union C
{
  union {
    struct {
      int i;
      int j;
    };
  };

  C () : i (3), j (2) {}
};
