// PR c++/41896
// { dg-options "-std=c++0x" }

void nested_lambda()
{
  float val;

  [val]()
    {
      [val](){};
    };
}
