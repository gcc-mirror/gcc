// PR c++/30111
// { dg-do run }

struct pod {
  int i;
};

struct inherit : pod {
  inherit() : pod() {}
};

int main()
{
  inherit i;
  return i.i != 0;
}

