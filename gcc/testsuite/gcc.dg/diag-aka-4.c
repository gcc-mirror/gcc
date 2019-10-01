typedef struct struct_wrapper { int i; } struct_wrapper;
typedef struct { int i; } anon_struct_wrapper;

typedef union union_wrapper { int i; } union_wrapper;
typedef union { int i; } anon_union_wrapper;

typedef enum enum_wrapper { A, B } enum_wrapper;
typedef enum { C, D } anon_enum_wrapper;

void test_struct_wrapper (struct_wrapper y, int x)
{
  struct_wrapper *ptr = &x; /* { dg-error {initialization of 'struct_wrapper \*' from incompatible pointer type 'int \*'} } */
  const struct_wrapper *const_ptr = &x; /* { dg-error {initialization of 'const struct_wrapper \*' from incompatible pointer type 'int \*'} } */
  volatile struct_wrapper *volatile_ptr = &x; /* { dg-error {initialization of 'volatile struct_wrapper \*' from incompatible pointer type 'int \*'} } */
  struct_wrapper (*aptr)[10] = &x; /* { dg-error {initialization of 'struct_wrapper \(\*\)\[10\]' from incompatible pointer type 'int \*'} } */
  struct_wrapper (*f1)(int) = &x; /* { dg-error {initialization of 'struct_wrapper \(\*\)\(int\)' from incompatible pointer type 'int \*'} } */
  int (*f2)(struct_wrapper) = &x; /* { dg-error {initialization of 'int \(\*\)\(struct_wrapper\)' from incompatible pointer type 'int \*'} } */
  y = x; /* { dg-error {incompatible types when assigning to type 'struct_wrapper' from type 'int'} } */
}

void test_anon_struct_wrapper (anon_struct_wrapper y, int x)
{
  anon_struct_wrapper *ptr = &x; /* { dg-error {initialization of 'anon_struct_wrapper \*' from incompatible pointer type 'int \*'} } */
  const anon_struct_wrapper *const_ptr = &x; /* { dg-error {initialization of 'const anon_struct_wrapper \*' from incompatible pointer type 'int \*'} } */
  volatile anon_struct_wrapper *volatile_ptr = &x; /* { dg-error {initialization of 'volatile anon_struct_wrapper \*' from incompatible pointer type 'int \*'} } */
  anon_struct_wrapper (*aptr)[10] = &x; /* { dg-error {initialization of 'anon_struct_wrapper \(\*\)\[10\]' from incompatible pointer type 'int \*'} } */
  anon_struct_wrapper (*f1)(int) = &x; /* { dg-error {initialization of 'anon_struct_wrapper \(\*\)\(int\)' from incompatible pointer type 'int \*'} } */
  int (*f2)(anon_struct_wrapper) = &x; /* { dg-error {initialization of 'int \(\*\)\(anon_struct_wrapper\)' from incompatible pointer type 'int \*'} } */
  y = x; /* { dg-error {incompatible types when assigning to type 'anon_struct_wrapper' from type 'int'} } */
}

void test_union_wrapper (union_wrapper y, int x)
{
  union_wrapper *ptr = &x; /* { dg-error {initialization of 'union_wrapper \*' from incompatible pointer type 'int \*'} } */
  const union_wrapper *const_ptr = &x; /* { dg-error {initialization of 'const union_wrapper \*' from incompatible pointer type 'int \*'} } */
  volatile union_wrapper *volatile_ptr = &x; /* { dg-error {initialization of 'volatile union_wrapper \*' from incompatible pointer type 'int \*'} } */
  union_wrapper (*aptr)[10] = &x; /* { dg-error {initialization of 'union_wrapper \(\*\)\[10\]' from incompatible pointer type 'int \*'} } */
  union_wrapper (*f1)(int) = &x; /* { dg-error {initialization of 'union_wrapper \(\*\)\(int\)' from incompatible pointer type 'int \*'} } */
  int (*f2)(union_wrapper) = &x; /* { dg-error {initialization of 'int \(\*\)\(union_wrapper\)' from incompatible pointer type 'int \*'} } */
  y = x; /* { dg-error {incompatible types when assigning to type 'union_wrapper' from type 'int'} } */
}

void test_anon_union_wrapper (anon_union_wrapper y, int x)
{
  anon_union_wrapper *ptr = &x; /* { dg-error {initialization of 'anon_union_wrapper \*' from incompatible pointer type 'int \*'} } */
  const anon_union_wrapper *const_ptr = &x; /* { dg-error {initialization of 'const anon_union_wrapper \*' from incompatible pointer type 'int \*'} } */
  volatile anon_union_wrapper *volatile_ptr = &x; /* { dg-error {initialization of 'volatile anon_union_wrapper \*' from incompatible pointer type 'int \*'} } */
  anon_union_wrapper (*aptr)[10] = &x; /* { dg-error {initialization of 'anon_union_wrapper \(\*\)\[10\]' from incompatible pointer type 'int \*'} } */
  anon_union_wrapper (*f1)(int) = &x; /* { dg-error {initialization of 'anon_union_wrapper \(\*\)\(int\)' from incompatible pointer type 'int \*'} } */
  int (*f2)(anon_union_wrapper) = &x; /* { dg-error {initialization of 'int \(\*\)\(anon_union_wrapper\)' from incompatible pointer type 'int \*'} } */
  y = x; /* { dg-error {incompatible types when assigning to type 'anon_union_wrapper' from type 'int'} } */
}

void test_enum_wrapper (enum_wrapper y, int x)
{
  enum_wrapper *ptr = &x; /* { dg-error {initialization of 'enum_wrapper \*' from incompatible pointer type 'int \*'} } */
  const enum_wrapper *const_ptr = &x; /* { dg-error {initialization of 'const enum_wrapper \*' from incompatible pointer type 'int \*'} } */
  volatile enum_wrapper *volatile_ptr = &x; /* { dg-error {initialization of 'volatile enum_wrapper \*' from incompatible pointer type 'int \*'} } */
  enum_wrapper (*aptr)[10] = &x; /* { dg-error {initialization of 'enum_wrapper \(\*\)\[10\]' from incompatible pointer type 'int \*'} } */
  enum_wrapper (*f1)(int) = &x; /* { dg-error {initialization of 'enum_wrapper \(\*\)\(int\)' from incompatible pointer type 'int \*'} } */
  int (*f2)(enum_wrapper) = &x; /* { dg-error {initialization of 'int \(\*\)\(enum_wrapper\)' from incompatible pointer type 'int \*'} } */
}

void test_anon_enum_wrapper (anon_enum_wrapper y, int x)
{
  anon_enum_wrapper *ptr = &x; /* { dg-error {initialization of 'anon_enum_wrapper \*' from incompatible pointer type 'int \*'} } */
  const anon_enum_wrapper *const_ptr = &x; /* { dg-error {initialization of 'const anon_enum_wrapper \*' from incompatible pointer type 'int \*'} } */
  volatile anon_enum_wrapper *volatile_ptr = &x; /* { dg-error {initialization of 'volatile anon_enum_wrapper \*' from incompatible pointer type 'int \*'} } */
  anon_enum_wrapper (*aptr)[10] = &x; /* { dg-error {initialization of 'anon_enum_wrapper \(\*\)\[10\]' from incompatible pointer type 'int \*'} } */
  anon_enum_wrapper (*f1)(int) = &x; /* { dg-error {initialization of 'anon_enum_wrapper \(\*\)\(int\)' from incompatible pointer type 'int \*'} } */
  int (*f2)(anon_enum_wrapper) = &x; /* { dg-error {initialization of 'int \(\*\)\(anon_enum_wrapper\)' from incompatible pointer type 'int \*'} } */
}
