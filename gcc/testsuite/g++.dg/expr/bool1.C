// { dg-do run }
// PR C++/29295
// make sure that a typedef for a bool will have the
//  the same results as a bool itself.

extern "C" void abort();
typedef bool my_bool;
int main()
{ 
  my_bool b = false;
  int i;

  b++; // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 13 }
  b++; // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 15 }
  i = b;
  if (i != 1)
    abort ();
  return 0;
}


