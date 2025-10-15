/* { dg-options "" } */

void multi1() [[gnu::aarch64_vector_pcs, gnu::preserve_none]]; /* { dg-warning {ignoring attribute 'preserve_none' because it conflicts} } */
void multi2() [[gnu::preserve_none, gnu::aarch64_vector_pcs]]; /* { dg-warning {ignoring attribute 'aarch64_vector_pcs' because it conflicts} } */

void normal_callee();
void preserve_none_callee() [[gnu::preserve_none]];
void vector_callee() [[gnu::aarch64_vector_pcs]];
void sve_callee(__SVBool_t);
void sve_preserve_none_callee(__SVBool_t) [[gnu::preserve_none]];

void (*normal_ptr)();
void (*preserve_none_ptr)() [[gnu::preserve_none]];
void (*vector_ptr)() [[gnu::aarch64_vector_pcs]];
void (*sve_ptr)(__SVBool_t);
void (*sve_preserve_none_ptr)(__SVBool_t) [[gnu::preserve_none]];

void f()
{
  normal_ptr = normal_callee;
  normal_ptr = preserve_none_callee; /* { dg-error {incompatible pointer type} } */
  normal_ptr = vector_callee; /* { dg-error {incompatible pointer type} } */
  normal_ptr = sve_callee; /* { dg-error {incompatible pointer type} } */
  normal_ptr = sve_preserve_none_callee; /* { dg-error {incompatible pointer type} } */

  preserve_none_ptr = normal_callee; /* { dg-error {incompatible pointer type} } */
  preserve_none_ptr = preserve_none_callee;
  preserve_none_ptr = vector_callee; /* { dg-error {incompatible pointer type} } */
  preserve_none_ptr = sve_callee; /* { dg-error {incompatible pointer type} } */
  preserve_none_ptr = sve_preserve_none_callee; /* { dg-error {incompatible pointer type} } */

  vector_ptr = normal_callee; /* { dg-error {incompatible pointer type} } */
  vector_ptr = preserve_none_callee; /* { dg-error {incompatible pointer type} } */
  vector_ptr = vector_callee;
  vector_ptr = sve_callee; /* { dg-error {incompatible pointer type} } */
  vector_ptr = sve_preserve_none_callee; /* { dg-error {incompatible pointer type} } */

  sve_ptr = normal_callee; /* { dg-error {incompatible pointer type} } */
  sve_ptr = preserve_none_callee; /* { dg-error {incompatible pointer type} } */
  sve_ptr = vector_callee; /* { dg-error {incompatible pointer type} } */
  sve_ptr = sve_callee;
  sve_ptr = sve_preserve_none_callee; /* { dg-error {incompatible pointer type} } */

  sve_preserve_none_ptr = normal_callee; /* { dg-error {incompatible pointer type} } */
  sve_preserve_none_ptr = preserve_none_callee; /* { dg-error {incompatible pointer type} } */
  sve_preserve_none_ptr = vector_callee; /* { dg-error {incompatible pointer type} } */
  sve_preserve_none_ptr = sve_callee; /* { dg-error {incompatible pointer type} } */
  sve_preserve_none_ptr = sve_preserve_none_callee;
}
