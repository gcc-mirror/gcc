/* { dg-do run } */
/* { dg-require-effective-target static } */
/* { dg-options "-O -fno-tree-loop-optimize -fno-tree-ter -static" } */
unsigned int in[8 * 8] =
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 };

unsigned char out[8 * 8];

int
main (void)
{
  int i;
  for (i = 0; i < 8 * 4; i++)
    {
      out[i * 2] = (unsigned char) in[i * 2] + 1;
      out[i * 2 + 1] = (unsigned char) in[i * 2 + 1] + 2;
    }
  __asm__("":::"memory");
  for (i = 0; i < 8 * 4; i++)
    {
      if (out[i * 2] != in[i * 2] + 1
	  || out[i * 2 + 1] != in[i * 2 + 1] + 2)
	__builtin_abort ();
    }
  return 0;
}
