/* Test that functions passed to the comma operator are correctly converted
   to pointers.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */

void foo (void);
void (*fp) (void);
char x[sizeof (1, foo) == sizeof (fp) ? 1 : -1];
