/* { dg-lto-do link } */
/* { dg-lto-options { { -O -flto -fexceptions -fnon-call-exceptions --param allow-store-data-races=0 } } } */
/* { dg-require-effective-target exceptions } */

typedef struct { unsigned int e0 : 16; } s1;
typedef struct { unsigned int e0 : 16; } s2;
typedef struct { s1 i1; s2 i2; } io;

static io *i;

void f1 (void)
{
  s1 x0;
  i->i1 = x0;
}

int main ()
{
  f1 ();
  return 0;
}
