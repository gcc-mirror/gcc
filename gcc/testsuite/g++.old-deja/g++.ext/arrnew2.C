// { dg-do run }
// { dg-options "-w -fpermissive" }

int *foo = new int[1](42); // { dg-bogus "" }
int main ()
{
  return foo[0] != 42;
}
