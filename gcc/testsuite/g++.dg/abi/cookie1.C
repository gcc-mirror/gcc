// { dg-options "-fabi-version=0" }

void *operator new[](__SIZE_TYPE__, void *);

struct A {
    ~A(){}
};

int main()
{
  A * a = (A*) new char[20];
  A  * b = new(a) A[3];
  if (a != b)
    return 1;
}
