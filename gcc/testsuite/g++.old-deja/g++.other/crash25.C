// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

class X {
public:
  X();
  virtual ~X();
}

X::x()
{				// ERROR - 
}

X::~x()
{				// ERROR - 
}
