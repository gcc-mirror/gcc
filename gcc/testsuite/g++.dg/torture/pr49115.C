// { dg-do run }

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
