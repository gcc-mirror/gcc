/* { dg-do compile } */
union U 
{
  int x; 
  float y;
} __attribute__ ((__transparent_union__));
