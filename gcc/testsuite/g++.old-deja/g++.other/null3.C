// { dg-do assemble  }

void x()
{
 int* p = 1==0;	// { dg-warning "converting 'false' to pointer" "" { target { ! c++11 } } }
// { dg-error "cannot convert" "" { target { c++11 } } .-1 }
}
