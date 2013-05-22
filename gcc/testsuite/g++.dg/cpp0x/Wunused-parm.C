// PR c++/57211
// { dg-options "-std=c++11 -Wunused-parameter" }

template <class T> T&& move(T&);

struct A
{
  struct B
  {
    B& operator=(B&&);
  };

  B f;

  A& operator=(A&& p) = default;
};

int main()
{
  A a;
  A b;
  b = move(a);
}
