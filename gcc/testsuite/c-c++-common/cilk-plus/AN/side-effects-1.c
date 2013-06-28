/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

/* Test that the array index, limit, and stride are evaluated only
   once.  */

int array[1000];

int func1_times = 0;
int func2_times = 0;
int func3_times = 0;
int func1() { func1_times++; return 0; }
int func2() { func2_times++; return 0; }
int func3() { func3_times++; return 0; }

int main()
{
  array[func1() + 11 : func2() + 22 : func3() + 33] = 666;

  if (func1_times != 1
      || func2_times != 1
      || func3_times != 1)
    return 1;

  return 0;
}
