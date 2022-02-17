/* { dg-do compile } */

struct Q {
  int *arr1;
  int *arr2;
  int *arr3;
  int len;
};

struct R {
  struct Q qarr[5];
};

struct R2 {
  struct Q *qptr;
};

#pragma omp declare mapper (struct Q myq) map(myq.arr1[0:myq.len]) \
					  map(myq.arr2[0:myq.len]) \
					  map(myq.arr3[0:myq.len])

#pragma omp declare mapper (struct R myr) map(myr.qarr[2:3])

#pragma omp declare mapper (struct R2 myr2) map(myr2.qptr[2:3])

int main (int argc, char *argv[])
{
  struct R r;
  struct R2 r2;
  int N = 256;

#pragma omp target
/* { dg-message "sorry, unimplemented: user-defined mapper with non-unit length array section" "" { target *-*-* } .-1 } */
  {
    for (int i = 2; i < 5; i++)
      for (int j = 0; j < N; j++)
	{
	  r.qarr[i].arr1[j]++;
	  r2.qptr[i].arr2[j]++;
	}
  }
}

