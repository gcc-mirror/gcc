// Build don't link:
// Special g++ Options: -g -O -fkeep-inline-functions

class c {
public:
  ~c () { };
};

int
foo (const c& lhs)
{
  c str (lhs);
  return 0;
}
