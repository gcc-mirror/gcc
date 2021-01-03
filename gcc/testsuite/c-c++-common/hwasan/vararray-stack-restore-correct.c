/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-shouldfail "hwasan" } */

#include <stdio.h>

/* Testing that a function with outgoing arguments correctly decrements the
   stack pointer when a vararray goes out of scope.  */

const char *
other (int argc, int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l)
{
  const char ** other;
    {
      const char * test_array[argc];
      test_array[0] = "test string";
      test_array[argc - 1] = "hello";
      /* To prevent optimisation.  */
      printf("While the value stored in our test_array is: %s\n",
	     test_array[argc - 1]);
      other = test_array;
    }
  /* With the below function call (the one with many arguments), some of the
     arguments have to be put on the stack, which means we have to reserve some
     space on the stack for these arguments and that the VLA is stored at a
     position that is not the stack pointer. */
  printf("Hello there!\nOur numbers today are: %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n",
	 a, b, c, d, e, f, g, h, i, j, k, l);
  /* This should fail due to a bad read access.  */
  return other[0];
}

int
main ()
{
  int a, b, c, d, e, f, g, h, i, j, k, l;
  const char * retval = other (1, a, b, c, d, e, f, g, h, i, j, k, l);
  /* Numbers don't matter here, just want to ensure the program is reading them
     so we know they won't be optimised out.  */
  if (retval)
    return 1;
  return 10;
}
