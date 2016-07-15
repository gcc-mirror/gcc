// PR c++/71604
// { dg-do compile { target c++11 } }

void foo ()
{
  int a[2] = { 1, 2 }; 
  for (struct S { S (int) {} } S : a) // { dg-error "types may not be defined" }
    ;
}
