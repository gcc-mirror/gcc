// { dg-do assemble  }
// Origin: Jakub Jelinek <jakub@redhat.com>

class X {
public:
  X();
  virtual ~X();
}

X::x()
{				// { dg-error "" } 
}

X::~x()                         // { dg-error "" } 
{				
}
