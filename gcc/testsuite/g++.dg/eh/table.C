// { dg-do compile { target *-*-darwin* } }
// { dg-final { scan-assembler "GCC_except_table0" } }
void needed();
void unneeded();

class Bar
{
public:
  Bar() {}
  virtual ~Bar() {}

  void unneeded();
};

void needed()
{
	Bar b;
}

//#if 0
void unneeded()
{
	Bar b;
	b.unneeded();
}
//#endif

int main()
{
	needed();

	return 0;
}
