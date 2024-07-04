/* { dg-additional-options "-std=gnu89" } */

char_autoincr (b1, c)
    short *b1;
    short c;
{
  *b1 = c;
}
