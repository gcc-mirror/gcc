/* PR debug/43165 */
/* { dg-options "-g" } */

struct __attribute__((packed)) S
{
  unsigned char a;
  unsigned short b;
  unsigned short c;
  unsigned d : 24;
};

void 
foo (struct S p)
{
  for (; p.c; p.c++)
    ;
}
