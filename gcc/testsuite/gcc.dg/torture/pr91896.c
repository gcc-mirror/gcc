/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned int
zj (unsigned int et)
{
  signed char jr = 0;

  do {
    et *= 3;
    jr += 2;
  } while (jr >= 0);

  if (et == (unsigned int) jr)
    et = 0;

  return et;
}
