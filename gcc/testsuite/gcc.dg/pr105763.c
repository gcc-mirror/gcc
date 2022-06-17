/* { dg-do compile } */
/* { dg-options "-O3" } */

int rl2_decode_png_bit_depth;
int *rl2_decode_png_p_data;
void png_destroy_read_struct ();
int __attribute__((returns_twice)) _setjmp ();
void rl2_decode_png_row_pointers()
{
  unsigned sample_type = 0;
  _setjmp();
  switch (rl2_decode_png_bit_depth)
  case 6:
    sample_type = 7;
  png_destroy_read_struct();
  for (;;)
    switch (sample_type)
    case 3:
    case 5:
      *rl2_decode_png_p_data;
}
