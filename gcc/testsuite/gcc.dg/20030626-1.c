/* PR optimization/11210 */
/* Originator: Guido Classen <guido@clagi.de> */
/* Reduced testcase by Falk Hueffner <falk@debian.org> */
/* { dg-do compile } */
/* { dg-options "-O" } */

/* Verify that the constant expressions folder doesn't
   throw away the cast operation in the comparison.  */

struct str { 
  int head; 
  signed char data[8];
};

int foo(struct str t)
{
  return t.data[0] || (unsigned char) t.data[2] != 130; /* { dg-bogus "comparison is always 1" } */
}
