// PR c++/52819

typedef void (*cfi)(void*);

void function(int *a) {}

template<cfi Func>
void get() { Func(0); }

int main()
{
  get<(cfi)function>();  // { dg-error "" }
  return 0;
}
