// { dg-do assemble  }
// Build don't linK:

int main()
{ // { dg-error "" } invalid redeclaration of
  return 0;
}


int main(int, const char**)
{ // { dg-error "" } as
  return 0;
}
