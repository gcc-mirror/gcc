// { dg-do assemble  }
// g++ 1.37.1 bug 900322_01

// ** Old, obsolete commentary:
// **************************************************************************
// The ANSI C standard, in section 3.1.2.5 (first paragraph) differentiates
// types into three disjoint sets, i.e object types, function types, and
// incomplete types.

// Also in 3.1.2.5 (page 24) the standard says that the element type of
// an array type is an object type.

// Later in that same section the standard also notes that array types with
// unknown size are considered incomplete types (page 25).  (Struct & union
// types which have only been "forward declared" are also incomplete types.)

// Some experts infer this to mean that it is not legal to specify or to
// construct an array *type* whose element type is an incomplete type.

// This interpretation suggests that the statements indicated below contain
// errors.

// g++ fails to flag all of the indicated statements with errors (even when
// the -pedantic option is used).
// **************************************************************************

// The above commentary is wrong.  (jason 1998/11/13)
// In fact, the lines marked OK are well-formed; the prohibition is only
// against forming array types with multiple unknown bounds.  This prohibition
// is found in 8.3.4 [dcl.array].

// It is also ill-formed to create an object of incomplete type.

// keywords: incomplete types, arrays, element types

extern int extern_two_d [] [];		// { dg-error "" } invalid declaration
int tenative_two_d [] [];		// { dg-error "" } caught by g++
static int static_two_d [] [];		// { dg-error "" } caught by g++

int (*pointer_to_two_d)[][];		// { dg-error "" } invalid declaration

void function_0 (int arg [] []) {	// { dg-error "" } invalid declaration
}

typedef int int_one_d_type [];
typedef int_one_d_type int_two_d_type[];// { dg-error "" } invalid declaration

struct s;

extern struct s extern_s_array [10];	// OK
struct s tenative_s_array [10];		// { dg-error "" } object with incomplete type
static struct s static_s_array [10];	// { dg-error "" } object with incomplete type

struct s (*pointer_to_s_array) [];	// OK

void function_1 (struct s arg []) {	// OK
}

typedef struct s s_type;
typedef s_type s_one_d_type [10];	// OK

int main () { return 0; }
