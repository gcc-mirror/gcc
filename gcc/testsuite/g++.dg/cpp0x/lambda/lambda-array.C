// Test that array capture by copy works.
// { dg-options -std=c++0x }
// { dg-do run }

struct A
{
  int i;
  A(int i): i(i) {}
  A(const A& a): i(a.i+1) {}
};

int main()
{
  A ar[4][3] = { { 10, 20, 30 },
		 { 40, 50, 60 },
		 { 70, 80, 90 },
		 { 100, 110, 120 } };
  int i = [ar] { return ar[1][1]; }().i;
  return (i!= 52);
}
