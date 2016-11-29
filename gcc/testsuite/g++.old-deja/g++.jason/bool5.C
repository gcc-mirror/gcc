// { dg-do run  }
int main ()
{
  bool b = false;
  int i = b++; // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 5 }
  if (i != false || b != true)
    return 1;
  i = b++; // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 9 }
  if (i != true || b != true)
    return 1;
}
