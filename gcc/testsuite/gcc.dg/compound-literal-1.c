/* { dg-do compile } */

/* PR c/43248 */

__extension__ int foo(__SIZE_TYPE__ i)
{
  i ? : (void *){}; /* { dg-error "" } */
}

