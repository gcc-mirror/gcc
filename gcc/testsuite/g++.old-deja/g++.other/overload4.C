// { dg-do link  }

void f(int* const volatile * const * const*);
void f(int* const * const * const*) {}

int main()
{
  int*** ip;
  f(&ip);
}
