extern "C" void abort ();

struct A {
  struct stat {
    int x;
    stat (int j) { abort (); }
  };
  static int stat (double d) { return 0; }	// gets bogus error - cfront takes it
  static int zap () {
    stat (0);
    return stat (1);	// gets bogus error - this should work
  }
};

int main () {
  return A::zap ();
}
