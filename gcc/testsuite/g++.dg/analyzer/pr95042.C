// { dg-additional-options "-O1" }

class kz {
public:
  kz ();

private:
  int yu;
};

const kz vl;
kz ax;

void
c1 (bool va, bool ze)
{
  kz ny, fb = vl;

  if (va)
    {
      if (ze)
        ny = vl;

      fb = ny;
    }

  ax = fb;
}
