// Origin: PR c++/46824

class Incomplete;
struct Ptr
{
  operator Incomplete*();
};

int
main()
{
  Ptr p;
  *p;
}
