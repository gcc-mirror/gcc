// PR c++/15471
// { dg-do run }

struct myclass { 
  unsigned a; 
  union { 
    unsigned x; 
  }; 
}; 
 
int main () {
  myclass foo;
  unsigned myclass::* member = &myclass::x; 
  if (&(foo.*member) != &foo.x)
    return 1;
}
