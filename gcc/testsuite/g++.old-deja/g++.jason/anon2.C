// g++ should not complain about anonymous bitfields.
// Build don't link:

struct A
{
  int : 2;	 
};
