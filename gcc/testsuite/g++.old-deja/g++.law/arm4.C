// GROUPS passed ARM-compliance
// arm file
// Message-Id: <199301272139.AA25489@world.std.com>
// From: kol@world.std.com (Nikolay Yatsenko)
// Subject: g++ bug
// Date: Wed, 27 Jan 1993 16:39:10 -0500

extern "C" int printf(const char*,...);
int count = 0;

struct S {
  int i;
  S(int b)      {
    i = b;
    count++; }
};

int main(void)
{
  double a = 2.0;

  S x(int (a));
  if (count > 0)
    printf ("FAIL\n");
  else
    printf ("PASS\n");
  return 0;
}
