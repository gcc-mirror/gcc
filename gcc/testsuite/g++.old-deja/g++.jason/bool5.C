// { dg-do run { target c++14_down } }
// { dg-do compile { target c++17 } }
int main ()
{
  bool b = false;
  int i = b++; // { dg-warning "deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
  if (i != false || b != true)
    return 1;
  i = b++; // { dg-warning "deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
  if (i != true || b != true)
    return 1;
}
