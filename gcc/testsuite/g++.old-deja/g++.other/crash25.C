// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

class X {
public:
  X();
  virtual ~X();
}

X::x()	// { dg-error "return type|member function|semicolon" }
{
}

X::~x()	// { dg-error "expected class-name" }
{				
}
