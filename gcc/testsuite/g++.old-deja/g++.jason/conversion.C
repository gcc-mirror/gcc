// Bug: g++ doesn't find the conversion path from DPtr& to B*.
// Build don't link:

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
