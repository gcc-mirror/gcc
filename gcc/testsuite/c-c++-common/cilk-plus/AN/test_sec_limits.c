/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

#include <limits.h>
int A[16];
int a = 0;

int main () {
      if (__sec_reduce_max(A[0:0:2]) != INT_MIN)
	    a++;

        if (__sec_reduce_min(A[0:0:2]) != INT_MAX)
	      a++;

	  return a;
}


