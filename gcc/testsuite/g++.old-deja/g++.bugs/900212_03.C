// g++ 1.36.1 bug 900212_03

// g++ segfaults on any attempt to use the ->* operator.

// Cfront 2.0 passes this test.

// keywords: member pointers, operator->*

struct struct0 {
  int data_member;
  void function_member ();
};

void struct0::function_member ()
{
}

int i;

int struct0::*dmp;
void (struct0::*fmp) ();

struct0 *ptr;

void global_function_0 ()
{
  i = ptr->*dmp;				// causes segfault
  (ptr->*fmp) ();				// causes segfault
  // i = ptr->*(&struct0::data_member);		// too complicated for cfront
  // (ptr->*(&struct0::function_member)) ();	// too complicated for cfront
}

int main () { return 0; }
