/* The declarator in a function definition should be able to take the
   form of an attributed function declarator, not just a plain
   function declarator.  This was formerly allowed by some of the code
   but then the wrong constraint checks were made because other code
   didn't recognise the declarator as being that of the function
   definition.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk>.  */

int (__attribute__((const)) x) (a, b)
     int a;
     int b;
{
  return a + b;
}
