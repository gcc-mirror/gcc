// PR c++/29295
// { dg-do run { target c++14_down } }
// { dg-do compile { target c++17 } }
// make sure that a typedef for a bool will have the
//  the same results as a bool itself.

extern "C" void abort();
typedef volatile bool my_bool;
int main()
{ 
  my_bool b = false;
  int i;

  b++; // { dg-warning "3:use of an operand of type .bool. in .operator\\+\\+. is deprecated" "" { target { ! c++17 } } }
  // { dg-error "3:use of an operand of type .bool. in .operator\\+\\+. is forbidden" "" { target c++17 } .-1 }
  // { dg-warning ".volatile.-qualified type is deprecated" "" { target c++2a } .-2 }
  b++; // { dg-warning "3:use of an operand of type .bool. in .operator\\+\\+. is deprecated" "" { target { ! c++17 } } }
  // { dg-error "3:use of an operand of type .bool. in .operator\\+\\+. is forbidden" "" { target c++17 } .-1 }
  // { dg-warning ".volatile.-qualified type is deprecated" "" { target c++2a } .-2 }
  i = b;
  if (i != 1)
    abort ();
  return 0;
}


