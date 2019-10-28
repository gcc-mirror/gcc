#define IS_SYSTEM_HEADER
#include "aka5.h"

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

void f (s s1)
{
  ue1 = s1; // { dg-error {cannot convert 's' to 'user_enum' in assignment} }
  ue2 = s1; // { dg-error {cannot convert 's' to 'user_enum_copy' {aka 'user_enum'} in assignment} }
  ue_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum_ptr' {aka 'user_enum\*'} in assignment} }
  ue_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum\*' in assignment} }
  const_ue_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'const user_enum\*' in assignment} }
  const_ue_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'const user_enum_copy\*' {aka 'const user_enum\*'} in assignment} }
  volatile_ue_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'volatile user_enum\*' in assignment} }
  volatile_ue_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'volatile user_enum_copy\*' {aka 'volatile user_enum\*'} in assignment} }
  ue_array_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum \(\*\)\[10\]' in assignment} }
  ue_array_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum_copy \(\*\)\[10\]' {aka 'user_enum \(\*\)\[10\]'} in assignment} }
  ue_fn_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum \(\*\)\(\)' in assignment} }
  ue_fn_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_enum\)' in assignment} }
  ue_fn_ptr3 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_enum, \.\.\.\)' in assignment} }
  ue_fn_ptr4 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum_copy \(\*\)\(\)' {aka 'user_enum \(\*\)\(\)'} in assignment} }
  ue_fn_ptr5 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_enum_copy\)' {aka 'void \(\*\)\(user_enum\)'} in assignment} }
  ue_fn_ptr6 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_enum_copy, \.\.\.\)' {aka 'void \(\*\)\(user_enum, \.\.\.\)'} in assignment} }
  unsafe_ue_fn_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)' in assignment} }
  unsafe_ue_fn_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_enum_copy \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)' {aka 'user_enum \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)'} in assignment} }

  us1 = s1; // { dg-error {no match for 'operator=' in 'us1 = s1' \(operand types are 'user_struct' and 's'\)} }
  us2 = s1; // { dg-error {no match for 'operator=' in 'us2 = s1' \(operand types are 'user_struct_copy' {aka 'user_struct'} and 's'\)} }
  us_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_struct_ptr' {aka 'user_struct\*'} in assignment} }
  us_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_struct\*' in assignment} }
  const_us_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'const user_struct\*' in assignment} }
  const_us_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'const user_struct_copy\*' {aka 'const user_struct\*'} in assignment} }

  uu1 = s1; // { dg-error {no match for 'operator=' in 'uu1 = s1' \(operand types are 'user_union' and 's'\)} }
  uu2 = s1; // { dg-error {no match for 'operator=' in 'uu2 = s1' \(operand types are 'user_union_copy' {aka 'user_union'} and 's'\)} }
  uu_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_union_ptr' {aka 'user_union\*'} in assignment} }
  uu_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_union\*' in assignment} }
  const_uu_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'const user_union\*' in assignment} }
  const_uu_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'const user_union_copy\*' {aka 'const user_union\*'} in assignment} }

  uv1 = s1; // { dg-error {cannot convert 's' to 'user_vector' in assignment} }
  uv2 = s1; // { dg-error {cannot convert 's' to 'user_vector_copy' {aka 'user_vector'} in assignment} }
  uv_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_vector_ptr' {aka 'user_vector\*'} in assignment} }
  uv_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_vector\*' in assignment} }
  const_uv_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'const user_vector\*' in assignment} }
  const_uv_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'const user_vector_copy\*' {aka 'const user_vector\*'} in assignment} }

  ui1 = s1; // { dg-error {cannot convert 's' to 'user_int' {aka 'int'} in assignment} }
  ui2 = s1; // { dg-error {cannot convert 's' to 'user_int_copy' {aka 'int'} in assignment} }
  ui_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_int_ptr' {aka 'int\*'} in assignment} }
  ui_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_int\*' {aka 'int\*'} in assignment} }
  const_ui_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'const user_int\*' {aka 'const int\*'} in assignment} }
  const_ui_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'const user_int_copy\*' {aka 'const int\*'} in assignment} }
  volatile_ui_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'volatile user_int\*' {aka 'volatile int\*'} in assignment} }
  volatile_ui_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'volatile user_int_copy\*' {aka 'volatile int\*'} in assignment} }
  ui_array_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_int \(\*\)\[10\]' {aka 'int \(\*\)\[10\]'} in assignment} }
  ui_array_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_int_copy \(\*\)\[10\]' {aka 'int \(\*\)\[10\]'} in assignment} }
  ui_fn_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_int \(\*\)\(\)' {aka 'int \(\*\)\(\)'} in assignment} }
  ui_fn_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_int\)' {aka 'void \(\*\)\(int\)'} in assignment} }
  ui_fn_ptr3 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_int, \.\.\.\)' {aka 'void \(\*\)\(int, \.\.\.\)'} in assignment} }
  ui_fn_ptr4 = &s1; // { dg-error {cannot convert 's\*' to 'user_int_copy \(\*\)\(\)' {aka 'int \(\*\)\(\)'} in assignment} }
  ui_fn_ptr5 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_int_copy\)' {aka 'void \(\*\)\(int\)'} in assignment} }
  ui_fn_ptr6 = &s1; // { dg-error {cannot convert 's\*' to 'void \(\*\)\(user_int_copy, \.\.\.\)' {aka 'void \(\*\)\(int, \.\.\.\)'} in assignment} }
  unsafe_ui_fn_ptr1 = &s1; // { dg-error {cannot convert 's\*' to 'user_int \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)' {aka 'int \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)'} in assignment} }
  unsafe_ui_fn_ptr2 = &s1; // { dg-error {cannot convert 's\*' to 'user_int_copy \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)' {aka 'int \(__attribute__\(\(transaction_unsafe\)\) \*\)\(\)'} in assignment} }
}
