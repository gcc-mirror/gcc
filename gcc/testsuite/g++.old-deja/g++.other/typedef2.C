//Build don't link:
struct S{

  static const int i;
  static const int j;
};

typedef S T;
const int T::i = 4;
const int T::j = 4;
