// { dg-do compile }
// { dg-options "-w -fpermissive" }

int *foo = new int[1](42); // { dg-error "parenthesized" "" { target c++17_down } }
int main ()
{
  return foo[0] != 42;
}
