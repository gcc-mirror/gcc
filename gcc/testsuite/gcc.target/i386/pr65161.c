/* { dg-do compile } */
/* { dg-options "-O3 -fselective-scheduling2 -mtune=slm" } */

extern char data_ch[];

short
foo ()
{
  int i;
  short shortsum = 0;
  for (i = 0; i < 16; i++)
    shortsum += data_ch[i];
  return shortsum;
}
