/* Test the type of a component of a conditional expression between
   two structures is correct.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */

struct s { char c; } a, b;
int c;
char x[sizeof ((c ? a : b).c) == 1 ? 1 : -1];
