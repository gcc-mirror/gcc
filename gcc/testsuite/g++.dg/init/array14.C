// PR c++/12883
// Bug: Destructor of array object not called if no prior
// instantiation of the template has happened.

// { dg-do run }

int ret = 1;

template <int> struct X
{
  X(int) { }
  ~X() { ret = 0; }
}; 
 
int main()
{
  { 
    X<0> array[] = { 0 };
  }
  return ret;
}
