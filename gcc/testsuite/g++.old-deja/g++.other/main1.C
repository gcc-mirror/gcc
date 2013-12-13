// { dg-do compile }

int main()  // { dg-message "previous declaration" }
{
  return 0;
}


int main(int, const char**) // { dg-error "conflicting" }
{
  return 0;
}
