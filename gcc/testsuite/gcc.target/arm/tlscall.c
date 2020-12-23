/* Test non-duplication of tlscall insn */

/* { dg-do assemble } */
/* { dg-options "-O2 -fPIC -mtls-dialect=gnu2" } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "FDPIC does not support gnu2 TLS dialect" { arm*-*-uclinuxfdpiceabi } "*" "" } */
/* { dg-skip-if "-mpure-code and -fPIC incompatible" { *-*-* } { "-mpure-code" } } */

typedef struct _IO_FILE FILE;

extern int foo(void);
extern int bar(void);

void uuid__generate_time()
{
 static int has_init = 0;
 static __thread int state_fd = -2;
 static __thread FILE *state_f;

 if (!has_init) {
   foo();
   has_init = 1;
 }

 if (state_fd == -2) {
  if (!state_f) {
   state_fd = -1;
  }
 }
 if (state_fd >= 0) {
  while (bar() < 0) {}
 }

}
