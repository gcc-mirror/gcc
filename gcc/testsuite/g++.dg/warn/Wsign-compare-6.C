// PR c++/23608
// { dg-options "-Wsign-compare" }

#define FIVE 5

int main()
{
  int i = 5;
  int const ic = 5;

  i < 5u;  // { dg-warning "5:comparison between signed and unsigned" }
  ic < 5u;     
  FIVE < 5u;
}
