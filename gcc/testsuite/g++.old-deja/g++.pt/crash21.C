// Build don't link:
// Special g++ Options:

class Pooled
{
};

class RefCounted
{
};

class BrickExpressionBase : public RefCounted, public Pooled
{
};

template<unsigned Dim, class LHS, class RHS, class OP>
class BrickExpression : public BrickExpressionBase
{
};

template <unsigned Dim, class T>
void f()
{
  typedef BrickExpression<Dim, T, T, T> ExprT;
  ExprT(3).apply;
}
