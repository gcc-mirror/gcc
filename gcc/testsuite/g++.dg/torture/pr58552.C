// { dg-do compile }
// { dg-additional-options "-fcompare-debug" }

extern void fancy_abort () __attribute__ ((__noreturn__));
extern "C" {
    struct __jmp_buf_tag { };
    typedef struct __jmp_buf_tag jmp_buf[1];
    extern int _setjmp (struct __jmp_buf_tag __env[1]) throw ();
}
extern void *gfc_state_stack;
static jmp_buf eof_buf;
static void push_state ()
{
  if (!gfc_state_stack)
    fancy_abort ();
}
bool gfc_parse_file (void)
{
  int seen_program=0;
  if (_setjmp (eof_buf))
    return false;
  if (seen_program)
    goto duplicate_main;
  seen_program = 1;
  push_state ();
  push_state ();
duplicate_main:
  return true;
}
