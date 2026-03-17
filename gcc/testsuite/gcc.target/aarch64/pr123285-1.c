/* { dg-do run } */
/* { dg-options "-O3" } */
/* PR target/123285 */

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))

/* f used to allocate v30 to either a or b and the inline-asm
   would clobber the v30. */
[[gnu::noipa]]
BS_VEC(int, 8) f(BS_VEC(int, 8) a, BS_VEC(int, 8) b)
{
  a+=b;
  asm("movi v30.16b, 0":::"v30");
  a+=b;
  return a;
}
[[gnu::noipa]]
BS_VEC(int, 8) f1(BS_VEC(int, 8) a, BS_VEC(int, 8) b)
{
  a+=b;
  a+=b;
  return a;
}

int main()
{
  BS_VEC(int, 8) a = {0,1,2,3,4,5,6,7};
  BS_VEC(int, 8) b = {8,9,10,11,12,13,14};
  BS_VEC(int, 8) c0 = f(a,b);
  BS_VEC(int, 8) c1 = f1(a,b);
  for(int i=0;i<8;i++)
  if ( c0[i] != c1[i] )
    __builtin_abort ();
}


