// Build don't link: 
/*
Check whether a typedef for a basic type as a baseclass is diagnosed.
*/
typedef int an_int;
class bar : public an_int {}; // ERROR - not an aggregate 
