// { dg-do assemble  }
// Bug: g++ doesn't find the conversion path from DPtr& to B*.

class B {};
class D : public B {};
class DPtr
{
public:
  operator D*() const;
};

void foo (B* bp);
void bar (DPtr& dp)
{
  foo (dp);
}
