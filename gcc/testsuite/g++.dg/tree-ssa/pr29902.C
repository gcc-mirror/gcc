/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-O1 -fprefetch-loop-arrays -march=athlon" } */
/* { dg-require-effective-target ilp32 } */

int length1();
int g(int);
void f(int capacity_, char *old_storage)
{
  try {
      length1();
      int old_capacity = capacity_;
      capacity_ *= 2;
      g(capacity_);
      for (int i = 1; i < old_capacity; i++)
	old_storage[i] = old_storage[i - 1];
  } catch (...) {
      for (int i = 1; i < capacity_; i++){old_storage[i] = 0;}
  }
}
