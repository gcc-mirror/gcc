// Based on a testcase by Eric Dumazet <Eric.Dumazet@COSMOSBAY.COM>

extern "C" void abort ();

const char * const foo = ""; // foo is not NULL

int main() {
  if ((foo == 0) ? 0 : foo)  // so this should evaluate to `foo'
    return 0;
  else
    abort();
}
