/* { dg-additional-options "-std=gnu89" } */

m32 (a)
     int *a;
{
  a[1] = a[0];
}

m16 (a)
     short *a;
{
  a[1] = a[0];
}


m8 (a)
     char *a;
{
  a[1] = a[0];
}

