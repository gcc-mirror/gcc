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

  b++; // { dg-warning "deprecated" }
  b++; // { dg-warning "deprecated" }
  i = b;
  if (i != 1)
    abort ();
  return 0;
}


