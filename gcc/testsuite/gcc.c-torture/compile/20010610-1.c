/* Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu>

   Boolean types were not accepted as array sizes nor as switch
   quantities.  */

#include <stdbool.h>

int
main(void)
{
  bool arr[(bool)1];
  
  switch (arr[0])
    {
    default:;
    }

  return 0;
}
