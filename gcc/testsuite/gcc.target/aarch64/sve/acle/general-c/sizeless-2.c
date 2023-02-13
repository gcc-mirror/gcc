/* { dg-options "-std=gnu99 -msve-vector-bits=256" } */

#include <arm_sve.h>

typedef signed char int8x32_t __attribute__((__vector_size__ (32)));

/* Sizeless objects with global scope.  */

svint8_t global_sve_sc; /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */
static svint8_t local_sve_sc; /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */
extern svint8_t extern_sve_sc; /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */
__thread svint8_t tls_sve_sc; /* { dg-error {variables of type 'svint8_t' cannot have thread-local storage duration} } */
_Atomic svint8_t atomic_sve_sc; /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */

/* Sizeless arrays.  */

typedef svint8_t array_type[2]; /* { dg-error {array elements cannot have SVE type 'svint8_t'} } */
extern svint8_t extern_array[]; /* { dg-error {array elements cannot have SVE type 'svint8_t'} } */

/* Sizeless fields.  */

struct struct1 {
  svint8_t a; /* { dg-error {fields cannot have SVE type 'svint8_t'} } */
};

union union1 {
  svint8_t a; /* { dg-error {fields cannot have SVE type 'svint8_t'} } */
};

/* Pointers to sizeless types.  */

svint8_t *global_sve_sc_ptr;
svint8_t *invalid_sve_sc_ptr = &(svint8_t) {}; /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */

/* Sizeless arguments and return values.  */

void ext_consume_sve_sc (svint8_t);
void ext_consume_varargs (int, ...);
svint8_t ext_produce_sve_sc ();

/* Main tests for statements and expressions.  */

void
statements (int n)
{
  /* Local declarations.  */

  unsigned char va __attribute__((__vector_size__(2)));
  svint8_t sve_sc1, sve_sc2;
  _Atomic svint8_t atomic_sve_sc;
  int8x32_t gnu_sc1;
  svint16_t sve_sh1;
  static svint8_t local_static_sve_sc; /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */

  /* Layout queries.  */

  sizeof (svint8_t); /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */
  sizeof (sve_sc1); /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */
  sizeof (ext_produce_sve_sc ()); /* { dg-error {SVE type 'svint8_t' does not have a fixed size} } */
  _Alignof (svint8_t); /* { dg-error {SVE type 'svint8_t' does not have a defined alignment} } */
  _Alignof (sve_sc1); /* { dg-error {SVE type 'svint8_t' does not have a defined alignment} } */
  _Alignof (ext_produce_sve_sc ()); /* { dg-error {SVE type 'svint8_t' does not have a defined alignment} } */

  /* Initialization.  */

  svint8_t init_sve_sc1 = sve_sc1;
  svint8_t init_sve_sc2 = sve_sh1; /* { dg-error {incompatible types when initializing type 'svint8_t' using type 'svint16_t'} } */
  svint8_t init_sve_sc3 = {};

  int initi_a = sve_sc1; /* { dg-error {incompatible types when initializing type 'int' using type 'svint8_t'} } */
  int initi_b = { sve_sc1 }; /* { dg-error {incompatible types when initializing type 'int' using type 'svint8_t'} } */

  /* Compound literals.  */

  (svint8_t) {};
  (svint8_t) { sve_sc1 };

  (int) { sve_sc1 }; /* { dg-error {incompatible types when initializing type 'int' using type 'svint8_t'} } */

  /* Arrays.  */

  svint8_t array[2]; /* { dg-error {array elements cannot have SVE type 'svint8_t'} } */
  svint8_t zero_length_array[0]; /* { dg-error {array elements cannot have SVE type 'svint8_t'} } */
  svint8_t empty_init_array[] = {}; /* { dg-error {array elements cannot have SVE type 'svint8_t'} } */
  typedef svint8_t vla_type[n]; /* { dg-error {array elements cannot have SVE type 'svint8_t'} } */

  /* Assignment.  */

  n = sve_sc1; /* { dg-error {incompatible types when assigning to type 'int' from type 'svint8_t'} } */

  sve_sc1 = 0; /* { dg-error {incompatible types when assigning to type 'svint8_t' from type 'int'} } */
  sve_sc1 = sve_sc2;
  sve_sc1 = sve_sh1; /* { dg-error {incompatible types when assigning to type 'svint8_t' from type 'svint16_t'} } */

  /* Casting.  */

  (void) sve_sc1;
  (svint8_t) sve_sc1;

  /* Addressing and dereferencing.  */

  svint8_t *sve_sc_ptr = &sve_sc1;
  int8x32_t *gnu_sc_ptr = &gnu_sc1;
  sve_sc1 = *sve_sc_ptr;

  /* Pointer assignment.  */

  gnu_sc_ptr = sve_sc_ptr; /* { dg-warning {incompatible pointer type} } */
  sve_sc_ptr = gnu_sc_ptr; /* { dg-warning {incompatible pointer type} } */

  /* Pointer arithmetic.  */

  ++sve_sc_ptr; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  --sve_sc_ptr; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr++; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr--; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr += 0; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr += 1; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr -= 0; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr -= 1; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc_ptr - sve_sc_ptr; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  gnu_sc_ptr - sve_sc_ptr; /* { dg-error {invalid operands to binary -} } */
  sve_sc_ptr - gnu_sc_ptr; /* { dg-error {invalid operands to binary -} } */
  sve_sc1 = sve_sc_ptr[0]; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */
  sve_sc1 = sve_sc_ptr[1]; /* { dg-error {arithmetic on pointer to SVE type 'svint8_t'} } */

  /* Pointer comparison.  */

  sve_sc_ptr == &sve_sc1;
  sve_sc_ptr != &sve_sc1;
  sve_sc_ptr < &sve_sc1;
  sve_sc_ptr <= &sve_sc1;
  sve_sc_ptr > &sve_sc1;
  sve_sc_ptr >= &sve_sc1;
  gnu_sc_ptr == sve_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  gnu_sc_ptr != sve_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  gnu_sc_ptr < sve_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  gnu_sc_ptr <= sve_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  gnu_sc_ptr > sve_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  gnu_sc_ptr >= sve_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  sve_sc_ptr == gnu_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  sve_sc_ptr != gnu_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  sve_sc_ptr < gnu_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  sve_sc_ptr <= gnu_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  sve_sc_ptr > gnu_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */
  sve_sc_ptr >= gnu_sc_ptr; /* { dg-warning {comparison of distinct pointer types} } */

  /* Conditional expressions.  */

  0 ? sve_sc1 : sve_sc1;
  0 ? sve_sc1 : sve_sh1; /* { dg-error {type mismatch in conditional expression} } */
  0 ? sve_sc1 : 0; /* { dg-error {type mismatch in conditional expression} } */
  0 ? 0 : sve_sc1; /* { dg-error {type mismatch in conditional expression} } */
  0 ?: sve_sc1; /* { dg-error {type mismatch in conditional expression} } */
  0 ? sve_sc_ptr : sve_sc_ptr;
  0 ? sve_sc_ptr : gnu_sc_ptr; /* { dg-warning {pointer type mismatch} } */
  0 ? gnu_sc_ptr : sve_sc_ptr; /* { dg-warning {pointer type mismatch} } */

  /* Generic associations.  */

  _Generic (sve_sc1, default: 100);
  _Generic (1, svint8_t: 10, default: 20);

  /* Function arguments.  */

  ext_consume_sve_sc (sve_sc1);
  ext_consume_sve_sc (sve_sh1); /* { dg-error {incompatible type for argument 1 of 'ext_consume_sve_sc'} } */
  ext_consume_varargs (sve_sc1); /* { dg-error {incompatible type for argument 1 of 'ext_consume_varargs'} } */
  ext_consume_varargs (1, sve_sc1);

  /* Function returns.  */

  ext_produce_sve_sc ();
  sve_sc1 = ext_produce_sve_sc ();
  sve_sh1 = ext_produce_sve_sc (); /* { dg-error {incompatible types when assigning to type 'svint16_t' from type 'svint8_t'} } */

  /* Varargs processing.  */

  __builtin_va_list valist;
  __builtin_va_arg (valist, svint8_t);

  /* Statement expressions.  */

  ({ sve_sc1; });
  ({ svint8_t another_sve_sc = *sve_sc_ptr; another_sve_sc; });
}

/* Function parameters in definitions.  */

void
old_style (input_sve_sc) /* { dg-error {SVE type 'svint8_t' cannot be passed to an unprototyped function} } */
     svint8_t input_sve_sc;
{
  svint8_t sve_sc1 = input_sve_sc;
}

void
new_style_param (svint8_t input_sve_sc)
{
  svint8_t sve_sc1 = input_sve_sc;
}

/* Function return values in definitions.  */

svint8_t
good_return_sve_sc (svint8_t param)
{
  return param;
}

svint8_t
bad_return_sve_sc (svint16_t param)
{
  return param; /* { dg-error {incompatible types when returning type 'svint16_t' but 'svint8_t' was expected} } */
}
