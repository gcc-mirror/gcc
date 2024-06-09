// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-require-effective-target freorder }
// { dg-options "-O2 -freorder-blocks-and-partition -g" }

extern "C" void abort (void);
struct MyException {};
struct Data {
    int nr;
    Data() : nr(66) {}
};
Data __attribute__((noinline,noclone)) getData(int i)
{
  if (i) throw MyException();
  Data data;
  data.nr = i;
  return data;
}
int main(int, char **)
{
  Data data;
  try {
      data = getData(1);
  } catch (MyException& e) {
      if (data.nr != 66)
	abort ();
  }
}
