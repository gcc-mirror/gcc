// PR c++/93238

short s;
enum { zero };
int fn(int i)
{
  return s >> zero;
}
