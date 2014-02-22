// PR c++/39681

int main()
{
  int* p = new foo; // { dg-error "16:type" }
}
