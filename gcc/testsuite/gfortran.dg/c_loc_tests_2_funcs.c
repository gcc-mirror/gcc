double fabs (double);

typedef struct ctype
{
  int i;
  double x;
}ctype_t;

int test_scalar_address(int *ptr)
{
  /* The value in Fortran should be initialized to 100. */
  if(*ptr != 100)
    return 0;
  else
    return 1;
}

int test_array_address(int *int_array, int num_elements)
{
  int i = 0;

  for(i = 0; i < num_elements; i++)
    /* Fortran will init all of the elements to 100; verify that here. */
    if(int_array[i] != 100)
      return 0;

  /* all elements were equal to 100 */
  return 1;
}

int test_type_address(ctype_t *type_ptr)
{
  /* i was set to 100 by Fortran */
  if(type_ptr->i != 100)
    return 0;
  
  /* x was set to 1.0d0 by Fortran */
  if(fabs(type_ptr->x - 1.0) > 0.00000000)
    return 0;
  
  return 1;
}
