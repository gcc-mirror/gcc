// g++ 1.37.1 bug 900511_01

// g++ fails to properly apply user-defined type conversion operators
// in cases where is it not obvious that the given conversion is
// appropriate for the context (i.e. operator and other operands)
// where the conversion should take place.

// cfront 2.0 passes this test.

struct struct_1 {
  int member;

  operator int ();
};

struct_1::operator int ()
{
  return 0;
}

struct struct_2 {
  int member;

  operator float ();
};

struct_2::operator float ()
{
  return 0.0;
}

struct_1 struct_1_object;
struct_2 struct_2_object;
double d;

void test ()
{
  d = struct_2_object + struct_1_object;	// OK
  d = struct_1_object + struct_2_object;	// gets bogus error
}

int main () { return 0; }
