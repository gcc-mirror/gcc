// PR c++/54501
// { dg-options "" }

int main()
{
  int a[][0] = {0};  // { dg-error "too many" }
}
