// { dg-do assemble  }

template <class T> void f1() 
{ 
  struct foo { enum T2 {
    un, du, toi };
  }; 
}

void f2() { f1<int>(); }
