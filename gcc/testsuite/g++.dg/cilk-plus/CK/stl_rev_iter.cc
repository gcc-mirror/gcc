/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */


#include <vector>
#include <cstdio>
#include <iostream>
#include <algorithm>

using namespace std;


int main(void)
{
vector <int> array,array_serial;

for (int ii = -1; ii < 10; ii++)
{
  array.push_back(ii);
  array_serial.push_back(ii);
}
_Cilk_for (vector<int>::reverse_iterator iter4 = array.rbegin();
	   iter4 != array.rend(); iter4++)
{
  if (*iter4 == 0x8) {
    *iter4 = 9;
  }
}

_Cilk_for (vector<int>::reverse_iterator iter4 = array_serial.rbegin();
	   iter4 != array_serial.rend(); iter4++)
{
  if (*iter4 == 0x8) {
    *iter4 = 9;
  }
}
_Cilk_for (vector<int>::reverse_iterator iter2 = array.rbegin();
	   iter2 != array.rend();
	  iter2 += 1)
{
   if ((*iter2 == 0x4) || (*iter2 == 0x7)) {
    *iter2 = 0x3;
   }
}
for (vector<int>::reverse_iterator iter2 = array_serial.rbegin();
     iter2 != array_serial.rend();
	  iter2 += 1)
{
   if ((*iter2 == 0x4) || (*iter2 == 0x7)) {
    *iter2 = 0x3;
   }
}
sort (array.begin(), array.end());
sort (array_serial.begin(), array_serial.end());

vector <int>::iterator iter = array.begin ();
vector <int>::iterator iter_serial = array_serial.begin ();
while (iter != array.end () && iter_serial != array_serial.end ())
{
  if (*iter != *iter_serial)
    __builtin_abort ();
  iter++;
  iter_serial++;
}

return 0;
}
