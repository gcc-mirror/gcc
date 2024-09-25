// PR c++/116369
// { dg-do run { target c++11 } }

struct f{
  mutable int t;
};

const f &g = {1};

int main()
{
  g.t++;
}
