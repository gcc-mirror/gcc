namespace {
  int i = 10;
  enum fish { one = 1, two = 2, red = 3, blue = 4 };
}
extern "C" void printf (const char *, ...);
int main(void)
{
  if (i != 10) {
    return 1;
  }
  if (one != 1) {
    return 2;
  }
}
