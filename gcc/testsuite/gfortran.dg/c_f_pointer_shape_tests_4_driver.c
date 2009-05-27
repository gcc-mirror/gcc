#define NUM_ELEMS 10
#define NUM_ROWS 2
#define NUM_COLS 3

void test_long_long_1d(int *array, int num_elems);
void test_long_long_2d(int *array, int num_rows, int num_cols);
void test_long_1d(int *array, int num_elems);
void test_int_1d(int *array, int num_elems);
void test_short_1d(int *array, int num_elems);
void test_mixed(int *array, int num_elems);

int main(int argc, char **argv)
{
  int my_array[NUM_ELEMS];
  int my_2d_array[NUM_ROWS][NUM_COLS];
  int i, j;

  for(i = 0; i < NUM_ELEMS; i++)
    my_array[i] = i;

  for(i = 0; i < NUM_ROWS; i++)
    for(j = 0; j < NUM_COLS; j++)
      my_2d_array[i][j] = (i*NUM_COLS) + j;

  /* Test c_f_pointer where SHAPE is of type integer, kind=c_long_long.  */
  test_long_long_1d(my_array, NUM_ELEMS);

  /* Test c_f_pointer where SHAPE is of type integer, kind=c_long_long.  
     The indices are transposed for Fortran.  */
  test_long_long_2d(my_2d_array[0], NUM_COLS, NUM_ROWS);

  /* Test c_f_pointer where SHAPE is of type integer, kind=c_long.  */
  test_long_1d(my_array, NUM_ELEMS);

  /* Test c_f_pointer where SHAPE is of type integer, kind=c_int.  */
  test_int_1d(my_array, NUM_ELEMS);

  /* Test c_f_pointer where SHAPE is of type integer, kind=c_short.  */
  test_short_1d(my_array, NUM_ELEMS);

  /* Test c_f_pointer where SHAPE is of type integer, kind=c_int and
	  kind=c_long_long.  */
  test_mixed(my_array, NUM_ELEMS);

  return 0;
}
