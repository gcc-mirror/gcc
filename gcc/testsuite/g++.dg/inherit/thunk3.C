// PR c++/18492

struct X{ ~X(); };
struct B
{
  virtual void a( X ) = 0;
}; 
struct D : public virtual B 
{
  void a( X );
}; 
void D::a( X ){}
