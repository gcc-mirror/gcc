/* PR c++/97201 - ICE in -Warray-bounds writing to result of operator new(0)
   Verify that out-of-bounds accesses to memory returned by the nothrow form
   of the new expression are diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -Warray-bounds -ftrack-macro-expansion=0" } */

#if 0
// Avoid including <new> to make cross-compiler testing easy.
// #include <new>
#else
namespace std {

typedef __SIZE_TYPE__ size_t;
struct nothrow_t { };
extern const nothrow_t nothrow;

}

void* operator new (std::size_t, const std::nothrow_t &) throw ()
  __attribute__  ((__alloc_size__ (1), __malloc__));
void* operator new[] (std::size_t, const std::nothrow_t &) throw ()
    __attribute__  ((__alloc_size__ (1), __malloc__));

#endif

typedef __INT32_TYPE__ int32_t;

void sink (void*);

template <int N> struct S { char a[N]; };

void sink (void*);

#define NEW(n)  new (std::nothrow) S<n>
#define T(T, n, i) do {				\
    T *p = (T*)NEW (n);				\
    p[i] = 0;					\
    sink (p);					\
  } while (0)

void warn_nothrow_new ()
{
  T (int32_t, 0, 0);          // { dg-warning "array subscript 0 is outside array bounds of 'int32_t \\\[0]'" }
                              // { dg-message "object of size \\d allocated by '\[^\n\r]*operator new\[^\n\r]*'" "note" { target *-*-* } .-1 }
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


void warn_nothrow_array_new ()
{
#undef NEW
#define NEW(n)  new (std::nothrow) char [n]

  T (int32_t, 0, 0);          // { dg-warning "array subscript 0 is outside array bounds of 'int32_t \\\[0]'" }
                              // { dg-message "object of size \\d allocated by '\[^\n\r]*operator new\[^\n\r]*'" "note" { target *-*-* } .-1 }
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
