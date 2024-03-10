/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

#include <string.h>

void
test_string_with_control_chars ()
{
  char buf[8];
  strcpy (buf, "\tone\n\ttwo\n"); /* { dg-warning "stack-based buffer overflow" } */
  /* { dg-warning "'__builtin_memcpy' writing 11 bytes into a region of size 8 overflows the destination" "" { target *-*-* } .-1 } */
}

/* { dg-begin-multiline-output "" }

  ┌──────┬──────┬──────┬─────┬─────┬─────┬─────┬─────┐┌─────┬─────┬──────┐
  │ [0]  │ [1]  │ [2]  │ [3] │ [4] │ [5] │ [6] │ [7] ││ [8] │ [9] │ [10] │
  ├──────┼──────┼──────┼─────┼─────┼─────┼─────┼─────┤├─────┼─────┼──────┤
  │ 0x09 │ 'o'  │ 'n'  │ 'e' │0x0a │0x09 │ 't' │ 'w' ││ 'o' │0x0a │ NUL  │
  ├──────┴──────┴──────┴─────┴─────┴─────┴─────┴─────┴┴─────┴─────┴──────┤
  │                  string literal (type: 'char[11]')                   │
  └──────────────────────────────────────────────────────────────────────┘
     │      │      │      │     │     │     │     │      │     │     │
     │      │      │      │     │     │     │     │      │     │     │
     v      v      v      v     v     v     v     v      v     v     v
  ┌──────┬─────────────────────────────────────┬─────┐┌──────────────────┐
  │ [0]  │                 ...                 │ [7] ││                  │
  ├──────┴─────────────────────────────────────┴─────┤│after valid range │
  │             'buf' (type: 'char[8]')              ││                  │
  └──────────────────────────────────────────────────┘└──────────────────┘
  ├────────────────────────┬─────────────────────────┤├────────┬─────────┤
                           │                                   │
                  ╭────────┴────────╮                ╭─────────┴─────────╮
                  │capacity: 8 bytes│                │overflow of 3 bytes│
                  ╰─────────────────╯                ╰───────────────────╯

   { dg-end-multiline-output "" } */
