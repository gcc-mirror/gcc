typedef struct {
    unsigned long bits;
} S;
struct T {
    S span;
    int flags;
};

struct T f(int x)
{
  return (struct T) {
      .span = (S) { 0UL },
      .flags = (x ? 256 : 0),
  };
}
