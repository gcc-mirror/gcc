// Build don't run:

void f(const int *);
void f(int *) {}

void f2(const volatile int *);
void f2(volatile int *) {}

int i;

int main()
{
  f(&i);
  f2(&i);
}
          
