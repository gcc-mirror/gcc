// { dg-do assemble  }
// GROUPS passed nested-classes
struct enclose {
  class nested;
  nested *nptr;
  class nested {
    int x;
  };
  void f();
};

void enclose::f()
{
  nptr = new enclose::nested;  
}

void g()
{
  enclose obj;
  obj.nptr = new enclose::nested;  
}
