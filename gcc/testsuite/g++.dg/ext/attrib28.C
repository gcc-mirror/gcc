// PR c++/28558
// { dg-options "" }

struct A
{
  A(int) { }
};

int main()
{
  A a = (A __attribute__((unused)))0; // { dg-warning "attribute" }
}
