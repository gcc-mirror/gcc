// g++ 1.37.1 bug 900519_04

// The following legal code causes g++ to segfault.

// cfront 2.0 passes this test.

// keywords: segfault, references, initialization

int cint_obj = 9;

void take_cint_ref (int& arg) { }

int& cint_ref_0 = cint_obj;
int& cint_ref_1 = cint_obj;

void test_0 ()
{
  take_cint_ref (cint_ref_1);	// causes segfault
}

int main () { return 0; }
