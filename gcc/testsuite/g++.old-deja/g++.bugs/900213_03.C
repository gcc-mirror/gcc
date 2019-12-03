// { dg-do assemble  }
// g++ 1.36.1 bug 900213_03

// g++ fails to detect an error when the address of a "bound" function is
// assigned to a pointer-to-member-function variable.

// It does however correctly detect a similar errors for data-members.

// keywords: bound function, operator&, member pointers

struct struct0 {
  int data_member;
  int function_member ();
};

int i;
int struct0::*dmp;
int (struct0::*fmp) ();

struct0 *ptr;

void global_function_0 ()
{
  fmp = &ptr->function_member;	// { dg-error "15:ISO C\\+\\+ forbids taking the address of a bound member function" } 
  //dmp = &ptr->data_member;	//  caught by g++, missed by cfront
}

int main () { return 0; }
