// g++ 1.37.1 bug 900404_07

// It is illegal to use a cast to attempt to convert an object type
// to a non-scalar type (e.g. an array type).

// g++ fails to properly flag as errors such illegal uses of array types.

// keywords: array types, casts, type conversion

typedef int array_type[10];

array_type *ap;

void foo ()
{
  int i = *((array_type) *ap);	/* ERROR - missed */
}
