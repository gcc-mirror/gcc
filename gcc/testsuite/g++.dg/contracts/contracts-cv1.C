// Tests to ensure that contracts have a properly cv qualified this
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct S
{
  int g() const { return x_; }
  int f() { return x_; }

  void mem_c() const
    [[ pre: f() ]] // { dg-error "discards qualifiers" }
  {
  }
  void mem_nc()
    [[ pre: f() ]]
  {
  }

  void memc_c() const
    [[ pre: g() ]]
  {
  }
  void memc_nc()
    [[ pre: g() ]]
  {
  }

  private:
    int x_{-10};
};

int main(int, char**)
{
  S s;
  return 0;
};

