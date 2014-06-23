// PR c++/59296
// { dg-do compile { target c++11 } }

struct Type
{
  void get() const& { }
  void get() const&& { }
};

int main()
{
  Type{}.get();
}
