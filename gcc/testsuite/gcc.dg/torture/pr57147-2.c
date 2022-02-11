/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-require-effective-target indirect_jumps } */

struct __jmp_buf_tag {};
typedef struct __jmp_buf_tag jmp_buf[1];
extern int _setjmp (struct __jmp_buf_tag __env[1]);

jmp_buf g_return_jmp_buf;

static void SetNaClSwitchExpectations (void)
{
  __builtin_longjmp (g_return_jmp_buf, 1);
}
void TestSyscall(void)
{
  SetNaClSwitchExpectations();

  _setjmp (g_return_jmp_buf);
}

/* sizeof g_return_jmp_buf is zero size but it's being passed to
   a function declared to take an array of one element which might
   trigger a warning.  Prune it out:
   { dg-prune-output "-Wstringop-overflow" } */

/* { dg-final { scan-tree-dump-not "setjmp" "optimized" } } */
