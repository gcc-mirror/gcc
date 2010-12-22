// PR c++/47003

struct A
{
  A(int);
};

volatile int i;

int main()
{
  A *q = new A (i);
}
