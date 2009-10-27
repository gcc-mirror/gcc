// { dg-options -std=c++0x }

int main()
{
  void (*pfn)() = []{};
}
