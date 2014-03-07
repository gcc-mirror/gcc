// PR c++/41896
// { dg-do compile { target c++11 } }

void nested_lambda()
{
  float val;

  [val]()
    {
      [val](){};
    };
}
