enum { a = 1 };

int main(void)
{
  int l = -1;

  return ! (l < a);		// testcase fails if a is unsigned
}
