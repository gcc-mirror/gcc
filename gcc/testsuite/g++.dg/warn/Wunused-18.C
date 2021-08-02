// PR c++/14199
// { dg-options "-W -Wall -Wunused" }

struct X { 
    static void foo (); 
}; 
 
template <typename T> 
void foo (const T &t) { 
  t.foo(); 
}

template void foo (const X &); 
