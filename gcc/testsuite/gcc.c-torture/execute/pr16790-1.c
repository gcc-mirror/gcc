/* PR middle-end/16790.  */

extern void abort ();

static void test1(unsigned int u1)
{
  unsigned int y_final_1;
  signed short y_middle;
  unsigned int y_final_2;

  y_final_1 = (unsigned int)( (signed short)(u1 * 2) * 3 );
  y_middle  =                 (signed short)(u1 * 2);
  y_final_2 = (unsigned int)( y_middle * 3 );

  if (y_final_1 != y_final_2)
    abort ();
}


static void test2(unsigned int u1)
{
  unsigned int y_final_1;
  signed short y_middle;
  unsigned int y_final_2;

  y_final_1 = (unsigned int)( (signed short)(u1 << 1) * 3 );
  y_middle  =                 (signed short)(u1 << 1);
  y_final_2 = (unsigned int)( y_middle * 3 );

  if (y_final_1 != y_final_2)
    abort ();
}


int main()
{
  test1(0x4000U);
  test2(0x4000U);
  return 0;
}

