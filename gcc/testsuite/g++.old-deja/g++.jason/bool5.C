// { dg-do run  }
int main ()
{
  bool b = false;
  int i = b++; // { dg-warning "deprecated" }
  if (i != false || b != true)
    return 1;
  i = b++; // { dg-warning "deprecated" }
  if (i != true || b != true)
    return 1;
}
