// Build don't run:

struct S {};

struct T : public S {};

struct U : public T {};

void f(int T::*) {}
void f(int U::*);

void g(void (T::*)(int)) {}
void g(void (U::*)(int));

int main()
{
  int S::*ip;
  void (S::*fp)(int);

  f(ip);
  g(fp);
}
