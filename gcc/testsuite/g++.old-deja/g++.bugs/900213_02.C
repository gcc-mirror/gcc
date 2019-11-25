// { dg-do assemble  }
// g++ 1.36.1 bug 900213_02

// The following erroneous code causes g++ to abort.

// Cfront 2.0 passes this test.

// keywords: abort, member pointers, operator*

struct struct0 {
  int data_member;
};

int i;
int struct0::*dmp;

void global_function_0 ()
{
  i = *dmp;			// { dg-error "7:invalid use of unary '\\\*' on pointer to member" } causes abort
}

int main () { return 0; }
