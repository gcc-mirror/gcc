/* This wrongly caused duplicate definitions of x in the assembler
   output.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */

static int x = 1;
void f (void) { extern int x; }
