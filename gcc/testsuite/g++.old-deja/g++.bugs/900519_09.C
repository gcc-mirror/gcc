// g++ 1.37.1 bug 900519_09

// g++ allows the allocation of const objects via operator new even when
// these uses of operator new do not include initializations.

// This is inconsistant within the restrictions placed on the construction
// of class, struct, and union types which have constant members.

// Since there is no completely valid way of initializing such objects
// after the invocation of new, these cases should all be illegal.

// keywords: operator new, initialization, const qualifier

struct struct_0 {
  int member;
};

typedef const int const_int;
typedef const struct struct_0 const_struct_0;

void test ()
{
  new const int;		// ERROR - 
  new const_int;		// ERROR - 
  new const struct_0;		// ERROR - 
  new const_struct_0;		// ERROR - 
}

int main () { return 0; }
