// { dg-do run  }
// Make sure that bool bitfields promote to int properly.

struct F {
  bool b1 : 1;
  bool b2 : 7;
};

int main()
{
  F f = { true, true };

  if (int (f.b1) != 1)
    return 1;
}
