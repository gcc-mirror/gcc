// { dg-do run  }
void f (char *const &) { }
int main ()
{
  f ("hi");
}
