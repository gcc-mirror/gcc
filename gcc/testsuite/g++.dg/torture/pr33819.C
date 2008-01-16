/* { dg-do run } */

class s
{
public:
  s(long long aa) : a(aa), i1(0) { }
  long long id() const { return (this->a << 16) >> 16; }
  bool operator< (s sv) { return this->a < sv.id(); }
private:
  long long a : 48;
  int i1 : 16;
};
s g(1);
extern "C" void abort (void);
int
main(int, char**)
{
  if (g < (1LL << 38) - 1)
    return 0;
  abort ();
}

