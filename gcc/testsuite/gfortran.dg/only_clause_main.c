/* this is an f90 function */
void testOnly(int *cIntPtr);

int main(int argc, char **argv)
{
   int myCInt;

   myCInt = -11;
   testOnly(&myCInt);
   
   return 0;
}/* end main() */
