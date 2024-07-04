/* { dg-additional-options "-std=gnu89" } */

typedef struct { } empty_t;

f ()
{
  empty_t i;
  bar (i);
}
