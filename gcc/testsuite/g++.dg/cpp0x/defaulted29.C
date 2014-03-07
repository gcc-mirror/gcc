// PR c++/46696
// { dg-do compile { target c++11 } }

struct A
{
  A& operator= (A const&);
};

struct B
{
  A ar[1];
  B& operator= (B const&) = default;
};

int main()
{
  B x;
  B y;
  y = x;
}
