// { dg-do compile }
// Contributed by: Nick Savoiu <savoiu at ics dot uci dot edu>
// PR c++/14250: Incomplete type in switch statement

template <typename T> 
struct A {
  operator int();
};
 
struct C1 { 
  static A<void> t1; 
 
  void fun() 
  { 
   switch(t1) 
   { 
    default: break; 
   } 
  } 
}; 
