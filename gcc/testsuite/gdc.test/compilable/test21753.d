// https://issues.dlang.org/show_bug.cgi?id=21753

struct Sample {
  int function() func1;
  int function() func2;
}

void noth(Sample smpl)() {
  static assert(smpl.func1() == 0);
  static assert(smpl.func2() == 1);
}

void main() {
  enum s = Sample(
      { return 0; },
      { return 1; }
  );
  static assert(s.func1() == 0);
  static assert(s.func2() == 1);
  noth!(s)();
}
