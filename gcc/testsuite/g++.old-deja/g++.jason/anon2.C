// { dg-do assemble  }
// g++ should not complain about anonymous bitfields.

struct A
{
  int : 2;	 
};
