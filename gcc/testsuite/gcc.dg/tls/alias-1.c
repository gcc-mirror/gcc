/* { dg-do link } */
/* { dg-require-alias "" } */
/* { dg-require-visibility "" } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */
/* Test that encode_section_info handles the change from externally
   defined to locally defined (via hidden).   Extracted from glibc.  */

struct __res_state {
	char x[123];
};

extern __thread struct __res_state bar
  __attribute__ ((tls_model ("initial-exec")));

int main()
{
  bar.x[0] = 0;
  return 0;
}

__thread struct __res_state foo;
extern __thread struct __res_state bar
  __attribute__ ((alias ("foo")))
  __attribute__ ((visibility ("hidden")));
