/* PR rtl-optimization/14692  */

void assert_failed (void);           
void eidecpos_1 (unsigned char *pos, long n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      const unsigned char *dc_ptr1 = pos;
      pos--;
      if (dc_ptr1 - pos == 1)
	assert_failed ();
    }
}
