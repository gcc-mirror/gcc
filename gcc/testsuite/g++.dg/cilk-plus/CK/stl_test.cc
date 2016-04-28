/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */


#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <list>

using namespace std;


int main(int argc, char **argv)
{
  vector <int> number_list, number_list_serial;
  int new_number = 0;
  int no_elements = 0;

  if (argc != 2)
  {
    no_elements = 10;
  }


  number_list.clear();
  number_list_serial.clear();
  for (int ii = 0; ii < no_elements; ii++)
  {
    number_list.push_back(new_number);
    number_list_serial.push_back(new_number);
  }

  _Cilk_for (int jj = 0; jj < no_elements; jj++)
  {
    number_list[jj] = jj + no_elements;
  }
  for (int jj = 0; jj < no_elements; jj++)
  {
    number_list_serial[jj] = jj + no_elements;
  }

  for (int jj = 0; jj < no_elements; jj++)
    if (number_list_serial[jj] != number_list[jj])
      __builtin_abort ();

  return 0;
}
