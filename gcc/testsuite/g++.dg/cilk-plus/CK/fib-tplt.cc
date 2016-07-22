/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */

struct fib_struct
{
  int x;
  int *y;
  int z[3];
  struct fib_struct *ptr_next;
  struct fib_struct operator+(struct fib_struct &other) {
    struct fib_struct z ;
     z.x = (*this).x + (other.x);
    return z; 
  }
  struct fib_struct operator-(int other) {
    struct fib_struct z ;
    z.x = this->x - other;
    return z;
  }
  bool operator<(int number) {
   return (this->x < number);
  }
    
};

template <typename T>
T fib (T z) {
    if (z < 2) return z;
    T a = _Cilk_spawn fib<T>(z - 1);
    T b = fib<T>(z - 2);
    T c = a + b;
    return (a+b);
}


int sfib(int x)
{
  if (x < 2) return x;
  int a = sfib(x-1);
  int b = sfib(x-2);
  return (a+b);
}

int main () {
     int z = 30;
     int parallel_fib = fib<int>(z);
     int serial_fib = sfib(z);
    if (serial_fib != parallel_fib) 
      __builtin_abort ();
    
    return 0;
}
