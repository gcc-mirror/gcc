/* PR optimization/8275 */
/* Contributed by Volker Reichelt. */

unsigned int foo (unsigned int u)
{
  return (u >> 32) & 0xffff;
} 
