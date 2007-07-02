/* use 0 for NULL so no need for system header */

int test_c_assoc_0(void *my_c_ptr);
int test_c_assoc_1(void *my_c_ptr_1, void *my_c_ptr_2);
int test_c_assoc_2(void *my_c_ptr_1, void *my_c_ptr_2, int num_ptrs);
void verify_assoc(void *my_c_ptr_1, void *my_c_ptr_2);

extern void abort(void);

int main(int argc, char **argv)
{
   int i;
   int j;
   
   if(test_c_assoc_0(0) != 0)
      abort();

   if(test_c_assoc_0(&i) != 1)
      abort();

   if(test_c_assoc_1(0, 0) != 0)
      abort();

   if(test_c_assoc_1(0, &i) != 0)
      abort();
   
   if(test_c_assoc_1(&i, &i) != 1)
      abort();

   if(test_c_assoc_1(&i, 0) != 0)
      abort();

   if(test_c_assoc_1(&i, &j) != 0)
      abort();

   /* this should be associated, cause only testing 1 ptr (i) */
   if(test_c_assoc_2(&i, 0, 1) != 1)
      abort();

   /* this should be associated */
   if(test_c_assoc_2(&i, &i, 2) != 1)
      abort();

   /* this should not be associated (i) */
   if(test_c_assoc_2(&i, &j, 2) != 0)
      abort();

   /* this should be associated, cause only testing 1 ptr (i) */
   if(test_c_assoc_2(&i, &j, 1) != 1)
      abort();

   verify_assoc(&i, &i);
   
   return 0;
}/* end main() */
