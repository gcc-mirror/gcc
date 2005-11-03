/* This used to ICE due to a backend problem on s390.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

struct rgba
{
  unsigned char r;
  unsigned char g;
  unsigned char b;
  unsigned char a;
};

void g (struct rgba);

void f (void) 
{
  struct rgba x;

  x.r = 0;
  x.g = 128;
  x.b = 128;
  x.a = 26;

  g (x);
}

