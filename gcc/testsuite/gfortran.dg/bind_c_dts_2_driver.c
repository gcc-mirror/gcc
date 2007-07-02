typedef struct c_type_0
{
  int i;
  int *ptr;
  int array[3];
}c_type_0_t;

typedef struct c_type_1
{
  c_type_0_t nested_type;
  int *ptr;
  int j;
}c_type_1_t;

void sub0(c_type_1_t *c_type, int expected_i, int *expected_nested_ptr,
          int array_0, int array_1, int array_2, 
          int *expected_ptr, int expected_j);

int main(int argc, char **argv)
{
  c_type_1_t c_type;

  c_type.nested_type.i = 10;
  c_type.nested_type.ptr = &(c_type.nested_type.i);
  c_type.nested_type.array[0] = 1;
  c_type.nested_type.array[1] = 2;
  c_type.nested_type.array[2] = 3;
  c_type.ptr = &(c_type.j);
  c_type.j = 11;
  
  sub0(&c_type, c_type.nested_type.i, c_type.nested_type.ptr, 
       c_type.nested_type.array[0],
       c_type.nested_type.array[1], c_type.nested_type.array[2], 
       c_type.ptr, c_type.j);
  
  return 0;
}
