// Build don't link:

typedef unsigned short ushort;
class foo {
public:
  static ushort a;
};
extern "C" {
typedef unsigned short ushort;
}
ushort foo::a;
static int baz()
{
  return foo::a;
}
