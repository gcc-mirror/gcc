/* { dg-do compile } */
/* { dg-require-ifunc "" }  */

void __attribute__ ((target("avx512vl"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512bw"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512dq"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512cd"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512vbmi"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512ifma"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512vpopcntdq"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512vbmi2"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("gfni"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("vpclmulqdq"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512vnni"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("avx512bitalg"))) foo () {} /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */
void __attribute__ ((target("default"))) foo () {}

int main()
{
  foo ();
  return 0;
}
