// { dg-do assemble  }
/*
Check whether a typedef for a basic type as a baseclass is diagnosed.
*/
typedef int an_int;
class bar : public an_int {}; // { dg-error "" } not an aggregate 
