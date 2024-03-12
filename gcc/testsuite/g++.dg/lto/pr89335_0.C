// { dg-lto-do link }
// { dg-lto-options { {-O2 -flto -Wsuggest-final-methods}} }
// { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" }
// { dg-require-linker-plugin "" }

class Container
{
public:
  virtual ~Container ();
};

class List : public Container
{
};

static List cache[256];

int main (void)
{
  return 0;
}
