// Testcase from TM TS.
// { dg-options -fgnu-tm }

extern "C" int printf (const char *, ...);

int f()
{
  static int i = 0;
  synchronized {
    printf("before %d\n", i);
    ++i;
    printf("after %d\n", i);
    return i;
  }
}
