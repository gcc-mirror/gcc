// PR c++/15142
// Bug: We were aborting after giving a warning about passing a non-POD.

// Suppress the warning about undefined behavior.
// { dg-options "-w" }

struct B { 
    B() throw() { } 
    B(const B&) throw() { } 
}; 
 
struct X { 
    B a; 
    X& operator=(const X&); 
}; 
 
struct S { S(...); }; 
 
void SillyFunc() { 
  throw S(X()); 
} 
