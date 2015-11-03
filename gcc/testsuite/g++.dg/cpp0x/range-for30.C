// PR c++/54430
// { dg-require-effective-target c++11 }

struct A
{
  A(int) {}
  int* begin() {return nullptr;}
  int* end() {return nullptr;}
};

int main()
{
  int i[] = { 1, 2, 3, 4 };
  for (int i : i);
  for (auto i : i);
  for (A v : v); // { dg-error "not declared in this scope" }
}
