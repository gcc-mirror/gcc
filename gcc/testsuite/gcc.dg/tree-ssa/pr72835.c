/* PR tree-optimization/72835.  */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target int32plus } */

struct struct_1 {
    unsigned int m1 : 6 ;
    unsigned int m2 : 24 ;
    unsigned int m3 : 6 ;
};

unsigned short var_32 = 0x2d10;

struct struct_1 s1;

void init ()
{
  s1.m1 = 4;
  s1.m2 = 0x7ca4b8;
  s1.m3 = 24;
}

void foo ()
{
  unsigned int c
    = ((unsigned int) s1.m2) * (-((unsigned int) s1.m3))
    + (var_32) * (-((unsigned int) (s1.m1)));
  if (c != 4098873984)
    __builtin_abort ();
}

int main ()
{
    init ();
    foo ();
    return 0;
}
