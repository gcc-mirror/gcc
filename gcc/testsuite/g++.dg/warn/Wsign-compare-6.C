// PR c++/23608
// { dg-options "-Wsign-compare" }

#define FIVE 5

int main()
{
  int i = 5;
  int const ic = 5;

  i < 5u;  // { dg-warning "5:comparison of integer expressions of different signedness" }
  ic < 5u;     
  FIVE < 5u;
}
