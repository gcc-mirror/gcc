// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

class X {
public:
  X();
  virtual ~X();
}

X::x()	// { dg-error "return type|member function" }
{
}

X::~x()	// { dg-error "expected class-name" }
{				
}
