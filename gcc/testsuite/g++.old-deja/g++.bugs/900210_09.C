// g++ 1.36.1 bug 900210_09

// g++ allows pointer to members (both data members and function members)
// to be implicitly converted to void*.

// Section 4.8 of the Cfront 2.0 Reference Manual disallows such implicit
// conversions.

// Cfront 2.0 passes this test.

// keywords: member pointers, void pointers, implicit type conversions

class class0 {
public:
  int class0_data_member_0;
  void class0_function_member_0 ();
};

int class0::*class0_data_member_pointer;
int (class0::*class0_function_member_pointer) ();

void *vp;

void global_function_0 ()
{
  vp = class0_data_member_pointer;		// ERROR - 
  vp = class0_function_member_pointer;		// ERROR - 
}

int main () { return 0; }


