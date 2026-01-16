/* PR c/123437 */
/* { dg-do compile } */

enum E { F };
typedef enum E V __attribute__ ((vector_size (16)));
V x, y, z;

void
foo ()
{
  z = x / y;
}
