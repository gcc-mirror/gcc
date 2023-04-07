// https://issues.dlang.org/show_bug.cgi?id=23589
struct TemplStr(string Description_) {}

template A() {
    bool member;
    alias THIS = typeof(this);
    static THIS staticInstance;
    static asSize()
    {
        return staticInstance.member;
    }
}

template B() {
    enum cols = columns();

    enum cols_two = cols;
    TemplStr!(cols_two) tstr;
}

struct S
{
  mixin A;
  mixin B;

  static string columns() {
    auto dummy = &asSize;
    return "as";
  }
}
