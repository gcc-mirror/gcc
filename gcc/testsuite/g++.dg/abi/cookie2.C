// { dg-options "-fabi-version=1" }

void *operator new[](__SIZE_TYPE__, void *);

struct A {
    ~A(){}
};

int main()
{
  A * a = (A*) new char[20];
  A  * b = new(a) A[3];
  // In the 3.2 ABI, a cookie was allocated in this case.
  if (a == b)
    return 1;
}
