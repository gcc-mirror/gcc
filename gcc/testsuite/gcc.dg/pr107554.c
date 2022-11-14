/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -foptimize-strlen" } */

#define ELEMS 0x40000000

int a[ELEMS];
int b[ELEMS];

int main()
{
  __builtin_memcpy(a, b, ELEMS*sizeof(int));
}
