/* { dg-do compile } */
/* { dg-options "-Os -fnon-call-exceptions -ftrapv" } */

struct s { ~s(); };
void
h (unsigned char *data, int c)
{
  s a1;
  while (c)
    {
      int m = *data++ << 8;
      m += *data++;
    }
}
