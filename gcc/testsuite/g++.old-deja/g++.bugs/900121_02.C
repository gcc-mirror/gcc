// g++ 1.36.1 bug 900121_02

// Assignment of structs is defined as memberwise assignment,
// however g++ (1.36.2) and Cfront 2.0 differ on the definition
// of assignment for unions.

// (NOTE: Stroustrup now says that assignment of unions which contain either
// members or sub-members (base classes are not allowed for unions) which
// have non-default assignment operators defined for them will be illegal
// in future.)

// g++ (1.36.2) on the other hand, accepts this program without errors.

// keywords: unions, operator=, inheritance, members

struct s0 {

  int i;

  void operator= (s0 & arg)
  {
    this->i = arg.i;
  }
};

struct s1 {

  double d;

  void operator= (s1 & arg)
  {
    this->d = arg.d;
  }
};

union u0 {
  s0 u0_member_0;		// ERROR - 
  s1 u0_member_1;		// ERROR - 
};

void function ()
{
  u0 u0_object_0;
  u0 u0_object_1;

  u0_object_0 = u0_object_1;
};

int main () { return 0; }
