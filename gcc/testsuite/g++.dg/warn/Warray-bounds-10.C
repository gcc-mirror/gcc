/* PR c++/97201 - ICE in -Warray-bounds writing to result of operator new(0)
   Verify that out-of-bounds accesses to memory returned by default operator
   new() are diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Warray-bounds -ftrack-macro-expansion=0" } */

typedef __INT32_TYPE__ int32_t;

void sink (void*);

#define OP_NEW(n)  operator new (n)
#define T(T, n, i) do {				\
    T *p = (T*) OP_NEW (n);			\
    p[i] = 0;					\
    sink (p);					\
  } while (0)

void warn_op_new ()
{
  T (int32_t, 0, 0);          // { dg-warning "array subscript 0 is outside array bounds of 'int32_t \\\[0]'" }
                              // { dg-message "referencing an object of size \\d allocated by 'void\\\* operator new\\\(\(long \)?unsigned int\\\)'" "note" { target *-*-* } .-1 }
  T (int32_t, 1, 0);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[0]' is partly outside array bounds of 'unsigned char \\\[1]'" }
  T (int32_t, 2, 0);         //  { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[0]' is partly outside array bounds of 'unsigned char \\\[2]'" }
  T (int32_t, 3, 0);         // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[0]' is partly outside array bounds of 'unsigned char \\\[3]'" }

  T (int32_t, 4, 0);

  T (int32_t, 0, 1);          // { dg-warning "array subscript 1 is outside array bounds of 'int32_t \\\[0]'" }
  T (int32_t, 1, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 2, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 3, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 4, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 5, 1);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[1]' is partly outside array bounds of 'unsigned char \\\[5]" }
  T (int32_t, 6, 1);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[1]' is partly outside array bounds of 'unsigned char \\\[6]" }
  T (int32_t, 7, 1);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[1]' is partly outside array bounds of 'unsigned char \\\[7]" }

  T (int32_t, 8, 1);
}


void warn_op_array_new ()
{
#undef OP_NEW
#define OP_NEW(n)  operator new[] (n)

  T (int32_t, 0, 0);          // { dg-warning "array subscript 0 is outside array bounds of 'int32_t \\\[0]'" }
                              // { dg-message "referencing an object of size \\d allocated by 'void\\\* operator new \\\[]\\\(\(long \)?unsigned int\\\)'" "note" { target *-*-* } .-1 }
  T (int32_t, 1, 0);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[0]' is partly outside array bounds of 'unsigned char \\\[1]'" }
  T (int32_t, 2, 0);         //  { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[0]' is partly outside array bounds of 'unsigned char \\\[2]'" }
  T (int32_t, 3, 0);         // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[0]' is partly outside array bounds of 'unsigned char \\\[3]'" }

  T (int32_t, 4, 0);

  T (int32_t, 0, 1);          // { dg-warning "array subscript 1 is outside array bounds of 'int32_t \\\[0]'" }
  T (int32_t, 1, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 2, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 3, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 4, 1);          // { dg-warning "array subscript 1 is outside array bounds " }
  T (int32_t, 5, 1);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[1]' is partly outside array bounds of 'unsigned char \\\[5]" }
  T (int32_t, 6, 1);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[1]' is partly outside array bounds of 'unsigned char \\\[6]" }
  T (int32_t, 7, 1);          // { dg-warning "array subscript 'int32_t {aka (long )?int}\\\[1]' is partly outside array bounds of 'unsigned char \\\[7]" }

  T (int32_t, 8, 1);
}
