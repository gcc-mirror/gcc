/* PR c/8588 */
/* Contributed by Volker Reichelt. */

/* Verify that GCC converts integer constants
   in shift operations. */
   
void foo()
{
  unsigned int i, j;
  j = (i >> 0xf0);
}
