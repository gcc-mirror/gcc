// { dg-do compile }

int main()  // { dg-error "previous declaration" }
{
  return 0;
}


int main(int, const char**) // { dg-error "conflicts" }
{
  return 0;
}
