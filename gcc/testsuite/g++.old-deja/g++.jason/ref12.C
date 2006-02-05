// { dg-do run  }
void f (const char *const &) { }
int main ()
{
  f ("hi");
}
