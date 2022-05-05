// https://issues.dlang.org/show_bug.cgi?id=3775

struct Bug3775 {
  static int byLine()() { return 1; }
}

static assert(cast(int)Bug3775.byLine == 1);
