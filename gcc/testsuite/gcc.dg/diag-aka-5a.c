#define IS_SYSTEM_HEADER
#include "diag-aka-5.h"

typedef user_enum user_enum_copy;

struct s { int i; };

user_enum ue1;
user_enum_copy ue2;
user_enum_ptr ue_ptr1;
user_enum *ue_ptr2;
const user_enum *const_ue_ptr1;
const user_enum_copy *const_ue_ptr2;
volatile user_enum *volatile_ue_ptr1;
volatile user_enum_copy *volatile_ue_ptr2;
__extension__ _Atomic user_enum *atomic_ue_ptr1;
__extension__ _Atomic user_enum_copy *atomic_ue_ptr2;
user_enum (*ue_array_ptr1)[10];
user_enum_copy (*ue_array_ptr2)[10];
user_enum (*ue_fn_ptr1) (void);
void (*ue_fn_ptr2) (user_enum);
void (*ue_fn_ptr3) (user_enum, ...);
user_enum_copy (*ue_fn_ptr4) (void);
void (*ue_fn_ptr5) (user_enum_copy);
void (*ue_fn_ptr6) (user_enum_copy, ...);
user_enum (*__attribute__((__transaction_unsafe__)) unsafe_ue_fn_ptr1) (void);
user_enum_copy (*__attribute__((__transaction_unsafe__)) unsafe_ue_fn_ptr2) (void);

user_struct us1;
user_struct_copy us2;
user_struct_ptr us_ptr1;
user_struct *us_ptr2;
const user_struct *const_us_ptr1;
const user_struct_copy *const_us_ptr2;

user_union uu1;
user_union_copy uu2;
user_union_ptr uu_ptr1;
user_union *uu_ptr2;
const user_union *const_uu_ptr1;
const user_union_copy *const_uu_ptr2;

user_vector uv1;
user_vector_copy uv2;
user_vector_ptr uv_ptr1;
user_vector *uv_ptr2;
const user_vector *const_uv_ptr1;
const user_vector_copy *const_uv_ptr2;

user_int ui1;
user_int_copy ui2;
user_int_ptr ui_ptr1;
user_int *ui_ptr2;
const user_int *const_ui_ptr1;
const user_int_copy *const_ui_ptr2;
volatile user_int *volatile_ui_ptr1;
volatile user_int_copy *volatile_ui_ptr2;
__extension__ _Atomic user_int *atomic_ui_ptr1;
__extension__ _Atomic user_int_copy *atomic_ui_ptr2;
user_int (*ui_array_ptr1)[10];
user_int_copy (*ui_array_ptr2)[10];
user_int (*ui_fn_ptr1) (void);
void (*ui_fn_ptr2) (user_int);
void (*ui_fn_ptr3) (user_int, ...);
user_int_copy (*ui_fn_ptr4) (void);
void (*ui_fn_ptr5) (user_int_copy);
void (*ui_fn_ptr6) (user_int_copy, ...);
user_int (*__attribute__((__transaction_unsafe__)) unsafe_ui_fn_ptr1) (void);
user_int_copy (*__attribute__((__transaction_unsafe__)) unsafe_ui_fn_ptr2) (void);

void f (struct s s)
{
  ue1 = s; /* { dg-error {assigning to type 'user_enum' from type 'struct s'} } */
  ue2 = s; /* { dg-error {assigning to type 'user_enum_copy' {aka 'user_enum'} from type 'struct s'} } */
  ue_ptr1 = &s; /* { dg-error {assignment to 'user_enum_ptr' {aka 'user_enum \*'} from incompatible pointer type 'struct s \*'} } */
  ue_ptr2 = &s; /* { dg-error {assignment to 'user_enum \*' from incompatible pointer type 'struct s \*'} } */
  const_ue_ptr1 = &s; /* { dg-error {assignment to 'const user_enum \*' from incompatible pointer type 'struct s \*'} } */
  const_ue_ptr2 = &s; /* { dg-error {assignment to 'const user_enum_copy \*' {aka 'const user_enum \*'} from incompatible pointer type 'struct s \*'} } */
  volatile_ue_ptr1 = &s; /* { dg-error {assignment to 'volatile user_enum \*' from incompatible pointer type 'struct s \*'} } */
  volatile_ue_ptr2 = &s; /* { dg-error {assignment to 'volatile user_enum_copy \*' {aka 'volatile user_enum \*'} from incompatible pointer type 'struct s \*'} } */
  atomic_ue_ptr1 = &s; /* { dg-error {assignment to '_Atomic user_enum \*' from incompatible pointer type 'struct s \*'} } */
  atomic_ue_ptr2 = &s; /* { dg-error {assignment to '_Atomic user_enum_copy \*' {aka '_Atomic user_enum \*'} from incompatible pointer type 'struct s \*'} } */
  ue_array_ptr1 = &s; /* { dg-error {assignment to 'user_enum \(\*\)\[10\]' from incompatible pointer type 'struct s \*'} } */
  ue_array_ptr2 = &s; /* { dg-error {assignment to 'user_enum_copy \(\*\)\[10\]' {aka 'user_enum \(\*\)\[10\]'} from incompatible pointer type 'struct s \*'} } */
  ue_fn_ptr1 = &s; /* { dg-error {assignment to 'user_enum \(\*\)\(void\)' from incompatible pointer type 'struct s \*'} } */
  ue_fn_ptr2 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_enum\)' from incompatible pointer type 'struct s \*'} } */
  ue_fn_ptr3 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_enum, \.\.\.\)' from incompatible pointer type 'struct s \*'} } */
  ue_fn_ptr4 = &s; /* { dg-error {assignment to 'user_enum_copy \(\*\)\(void\)' {aka 'user_enum \(\*\)\(void\)'} from incompatible pointer type 'struct s \*'} } */
  ue_fn_ptr5 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_enum_copy\)' {aka 'void \(\*\)\(user_enum\)'} from incompatible pointer type 'struct s \*'} } */
  ue_fn_ptr6 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_enum_copy, \.\.\.\)' {aka 'void \(\*\)\(user_enum, \.\.\.\)'} from incompatible pointer type 'struct s \*'} } */
  unsafe_ue_fn_ptr1 = &s; /* { dg-error {assignment to 'user_enum \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)' from incompatible pointer type 'struct s \*'} } */
  unsafe_ue_fn_ptr2 = &s; /* { dg-error {assignment to 'user_enum_copy \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)' {aka 'user_enum \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)'} from incompatible pointer type 'struct s \*'} } */

  us1 = s; /* { dg-error {assigning to type 'user_struct' from type 'struct s'} } */
  us2 = s; /* { dg-error {assigning to type 'user_struct_copy' {aka 'user_struct'} from type 'struct s'} } */
  us_ptr1 = &s; /* { dg-error {assignment to 'user_struct_ptr' {aka 'user_struct \*'} from incompatible pointer type 'struct s \*'} } */
  us_ptr2 = &s; /* { dg-error {assignment to 'user_struct \*' from incompatible pointer type 'struct s \*'} } */
  const_us_ptr1 = &s; /* { dg-error {assignment to 'const user_struct \*' from incompatible pointer type 'struct s \*'} } */
  const_us_ptr2 = &s; /* { dg-error {assignment to 'const user_struct_copy \*' {aka 'const user_struct \*'} from incompatible pointer type 'struct s \*'} } */

  uu1 = s; /* { dg-error {assigning to type 'user_union' from type 'struct s'} } */
  uu2 = s; /* { dg-error {assigning to type 'user_union_copy' {aka 'user_union'} from type 'struct s'} } */
  uu_ptr1 = &s; /* { dg-error {assignment to 'user_union_ptr' {aka 'user_union \*'} from incompatible pointer type 'struct s \*'} } */
  uu_ptr2 = &s; /* { dg-error {assignment to 'user_union \*' from incompatible pointer type 'struct s \*'} } */
  const_uu_ptr1 = &s; /* { dg-error {assignment to 'const user_union \*' from incompatible pointer type 'struct s \*'} } */
  const_uu_ptr2 = &s; /* { dg-error {assignment to 'const user_union_copy \*' {aka 'const user_union \*'} from incompatible pointer type 'struct s \*'} } */

  uv1 = s; /* { dg-error {assigning to type 'user_vector' from type 'struct s'} } */
  uv2 = s; /* { dg-error {assigning to type 'user_vector_copy' {aka 'user_vector'} from type 'struct s'} } */
  uv_ptr1 = &s; /* { dg-error {assignment to 'user_vector_ptr' {aka 'user_vector \*'} from incompatible pointer type 'struct s \*'} } */
  uv_ptr2 = &s; /* { dg-error {assignment to 'user_vector \*' from incompatible pointer type 'struct s \*'} } */
  const_uv_ptr1 = &s; /* { dg-error {assignment to 'const user_vector \*' from incompatible pointer type 'struct s \*'} } */
  const_uv_ptr2 = &s; /* { dg-error {assignment to 'const user_vector_copy \*' {aka 'const user_vector \*'} from incompatible pointer type 'struct s \*'} } */

  ui1 = s; /* { dg-error {assigning to type 'user_int' {aka 'int'} from type 'struct s'} } */
  ui2 = s; /* { dg-error {assigning to type 'user_int_copy' {aka 'int'} from type 'struct s'} } */
  ui_ptr1 = &s; /* { dg-error {assignment to 'user_int_ptr' {aka 'int \*'} from incompatible pointer type 'struct s \*'} } */
  ui_ptr2 = &s; /* { dg-error {assignment to 'user_int \*' {aka 'int \*'} from incompatible pointer type 'struct s \*'} } */
  const_ui_ptr1 = &s; /* { dg-error {assignment to 'const user_int \*' {aka 'const int \*'} from incompatible pointer type 'struct s \*'} } */
  const_ui_ptr2 = &s; /* { dg-error {assignment to 'const user_int_copy \*' {aka 'const int \*'} from incompatible pointer type 'struct s \*'} } */
  volatile_ui_ptr1 = &s; /* { dg-error {assignment to 'volatile user_int \*' {aka 'volatile int \*'} from incompatible pointer type 'struct s \*'} } */
  volatile_ui_ptr2 = &s; /* { dg-error {assignment to 'volatile user_int_copy \*' {aka 'volatile int \*'} from incompatible pointer type 'struct s \*'} } */
  atomic_ui_ptr1 = &s; /* { dg-error {assignment to '_Atomic user_int \*' {aka '_Atomic int \*'} from incompatible pointer type 'struct s \*'} } */
  atomic_ui_ptr2 = &s; /* { dg-error {assignment to '_Atomic user_int_copy \*' {aka '_Atomic int \*'} from incompatible pointer type 'struct s \*'} } */
  ui_array_ptr1 = &s; /* { dg-error {assignment to 'user_int \(\*\)\[10\]' {aka 'int \(\*\)\[10\]'} from incompatible pointer type 'struct s \*'} } */
  ui_array_ptr2 = &s; /* { dg-error {assignment to 'user_int_copy \(\*\)\[10\]' {aka 'int \(\*\)\[10\]'} from incompatible pointer type 'struct s \*'} } */
  ui_fn_ptr1 = &s; /* { dg-error {assignment to 'user_int \(\*\)\(void\)' {aka 'int \(\*\)\(void\)'} from incompatible pointer type 'struct s \*'} } */
  ui_fn_ptr2 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_int\)' {aka 'void \(\*\)\(int\)'} from incompatible pointer type 'struct s \*'} } */
  ui_fn_ptr3 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_int, \.\.\.\)' {aka 'void \(\*\)\(int, \.\.\.\)'} from incompatible pointer type 'struct s \*'} } */
  ui_fn_ptr4 = &s; /* { dg-error {assignment to 'user_int_copy \(\*\)\(void\)' {aka 'int \(\*\)\(void\)'} from incompatible pointer type 'struct s \*'} } */
  ui_fn_ptr5 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_int_copy\)' {aka 'void \(\*\)\(int\)'} from incompatible pointer type 'struct s \*'} } */
  ui_fn_ptr6 = &s; /* { dg-error {assignment to 'void \(\*\)\(user_int_copy, \.\.\.\)' {aka 'void \(\*\)\(int, \.\.\.\)'} from incompatible pointer type 'struct s \*'} } */
  unsafe_ui_fn_ptr1 = &s; /* { dg-error {assignment to 'user_int \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)' {aka 'int \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)'} from incompatible pointer type 'struct s \*'} } */
  unsafe_ui_fn_ptr2 = &s; /* { dg-error {assignment to 'user_int_copy \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)' {aka 'int \(__attribute__\(\(transaction_unsafe\)\) \*\)\(void\)'} from incompatible pointer type 'struct s \*'} } */
}
