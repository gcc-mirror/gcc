/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

static inline int __attribute__((__always_inline__))
foo ()
{
  return log_bad_request(0, __builtin_va_arg_pack()); /* { dg-warning "implicit" } */
}
void log_bad_request() { foo (0); } /* { dg-warning "conflicting types" } */
