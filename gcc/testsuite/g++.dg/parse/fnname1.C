// PR c++/64222

class A
{
public:
  A (const char *, void *);
};
class B
{
public:
  B (A);
};
void
fn1 ()
{
  B a (A (__func__, 0));
}
