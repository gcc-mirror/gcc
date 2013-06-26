/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#ifdef HAVE_IO
#include <stdio.h>
#endif


int func1(int x)
{
  /* If x == 2 then it should return 0.  */
  return (x - 2);
}

int func2(int x)
{
  /* If x == 2 then it should return 1000.  */
  return (x * 500);
}

int func3 (int x)
{
  /* If x == 2 then it should return 1.  */
  /* If x == 1 then it should return 0.  */
  return (x-1);
}

int func4(int x)
{
  if (x > 0) 
    return x;
  else 
    return x--;
}


/* This program makes an assumption that argc == 1.  */
int main (void)
{
  int argc = 1;
  int array[2500];

  /* This is done to make sure the compiler does not optimize out argc.  */
  __asm volatile ("" : "+r" (argc));
  /* This should set array[0->999] to 5.  */
  array[argc-1:func2(++argc):1] = 5;
  array[1000:500:1] = 10; /* set all variables in array[1000-->1499] to 10.  */
  array[1500:500:1] = 15; /* set all variables in array[1500-->1999] to 15.  */
  array[2000:500:1] = 20; /* set all variables in array[2000-->2499] to 20.  */
  array[2000:500:1] = 25; /* set all variables in array[2500-->2999] to 25.  */
  array[2000:500:1] = 30; /* set all variables in array[3000-->3499] to 30.  */
  
  argc = func3 (argc); /* This will set argc back to 1.  */
#if HAVE_IO
  printf("argc = %d\n", argc);
#endif
  /* If the parameters inside the function get evaluated only once, then this
     if statement must work fine, i.e. the triplet values will be 0, 1000, 1.

     Otherwise, the program should crash or give some uneasy value.  */

  /* If done correctly, it should boil down to: array[0:1000:1].  */
  if (array[func3(argc):func2(++argc)] != 5) {
#ifdef HAVE_IO
    printf ("Should not be there(1).\n");
#endif
    return 1;
  }
  
  /* If done correctly, it should boil down to: array[999:500:-1].  */
  if (func4(array[func2(argc)-1:func2(argc--):func1(argc)]) != 5) {
#ifdef HAVE_IO
    printf ("Should not be there(2).\n");
#endif
    return 2;
  }

  /* If done correctly, it should boil down to: array[1000:500:1].  */
  if (func4 (func4(array[func2(argc++):500: func1(argc--)])) != 5) {
#ifdef HAVE_IO
    printf ("Should not be there(3).\n");
#endif
    return 3;
  }

  return 0;
}
