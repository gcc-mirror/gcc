// { dg-do run  }
// g++ 1.36.1 bug 900220_03

// g++ does not properly disambiguate calls to overloaded functions
// which are nearly identical except that one take a reference to a
// type `T' object and another takes a reference to a type `const T'
// object.

// (Note that the volatile stuff is commented out here because cfront
// does not yet grok volatile.)

// Cfront 2.0 passes this test.

// keywords: references, overloading, type qualifiers, pointers

int c_call_count = 0;
int cc_call_count = 0;
//int vc_call_count = 0;

void overloaded (char&)
{
  c_call_count++;
}

void overloaded (const char&)
{
  cc_call_count++;
}

//void overloaded (volatile char&)
//{
//  vc_call_count++;
//}

int test ()
{
  char c = 0;
  const char cc = 0;
  //volatile char vc = 0;

  char& cr = c;
  const char& ccr = cc;
  //volatile char& vcr = vc;

  overloaded (c);		// OK
  overloaded (cc);		// { dg-bogus "" } 
  //overloaded (vc);		// OK

  return (c_call_count != 1 || cc_call_count != 1 /* || vc_call_count != 1 */);
}

int main () { return test (); }
