/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* After the open parenthesis before the __attribute__, we used to shift
   the __attribute__ (expecting a parenthesised abstract declarator)
   instead of reducing to the start of a parameter list.  */
void bar (int (__attribute__((__mode__(__SI__))) int foo));
