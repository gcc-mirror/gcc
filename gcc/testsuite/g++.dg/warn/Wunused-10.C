// PR c++/19190
// { dg-options "-Wunused" }

struct breakme 
{ 
  void setAction( unsigned char a ) { act = a; } 
  unsigned int act:8; 
}; 
