/* PR c/13382 */
/* Origin: Richard Hutchinson <richard.hutchinson@asa.co.uk> */

/* Verify that the null initializer is converted to the right
   pointer type.  */

/* { dg-do compile } */
/* { dg-options "-O" } */

struct t
{
  int aMember;
};

struct t *const aPointer = 0;

void foo()
{
  int anInt = (aPointer == 0) ? 0 : aPointer->aMember;
}
