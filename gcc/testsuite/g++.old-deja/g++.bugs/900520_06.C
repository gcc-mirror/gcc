// g++ 1.37.1 bug 900520_06

// When an object of a class type is passed into a formal parameter of the
// same class type (in a function call) the language definition calls for
// this action to be treated like any other form of an initialization of
// an object of the given class type.

// g++ fails however to invoke the (compiler-supplied) copy constructor for
// the class type when a parameter of the class type is passed as an
// actual parameter.

// This causes the following program to exit with a nonzero exit status.

// cfront 2.0 passes this test.

int base_copy_ctor_called = 0;
int member_copy_ctor_called = 0;

struct struct_0 {
  struct_0 ();
  struct_0 (const struct_0&);
};

struct_0::struct_0 ()
{
}

struct_0::struct_0 (const struct_0&)
{
  base_copy_ctor_called++;
}

struct struct_1 {
  struct_1 ();
  struct_1 (const struct_1&);
};

struct_1::struct_1 ()
{
}

struct_1::struct_1 (const struct_1&)
{
  member_copy_ctor_called++;
}

struct struct_2 : public struct_0 {
  struct_2 ();
  struct_1 struct_1_member;
#ifdef MAKE_COPY_CONSTRUCTOR_EXPLICIT
  struct_2 (const struct_2&);
#endif
};

struct_2::struct_2 ()
{
}

#ifdef MAKE_COPY_CONSTRUCTOR_EXPLICIT
struct_2::struct_2 (const struct_2& arg) :
  struct_0 ((struct_0&)arg),
  struct_1_member (arg.struct_1_member)
{
}
#endif

void take_struct_2 (struct_2 arg)
{
}

int test ()
{
  struct_2 struct_2_object0;
  take_struct_2 (struct_2_object0);
  return (base_copy_ctor_called != 1 || member_copy_ctor_called != 1);
}

int main () { return test (); }
