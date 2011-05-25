// PR c++/46696
// { dg-options -std=c++0x }

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
