/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC -g" } */

typedef unsigned short ush;
typedef ush Pos;
extern ush prev[];
void fill_window( unsigned more, unsigned m)
{
    unsigned n;
    for (n = 0; n < (unsigned)(1<<15); n++) {
      (prev+0x8000)[n] = (Pos)(m >= 0x8000 ? m-0x8000 : 0);
    }
    for (n = 0; n < 0x8000; n++) {
      prev[n] = (Pos)(m >= 0x8000 ? m-0x8000 : 0);
    }
}
