/* initialized by fortran */
int i;
void test_globals(void);

extern void abort(void);

int main(int argc, char **argv)
{
   /* verify that i has been initialized by f90 */
   if(i != 2)
      abort();
   test_globals();
   return 0;
}/* end main() */
