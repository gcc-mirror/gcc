/* Regression test for ICE with short values of
   --param=analyzer-text-art-string-ellipsis-threshold=.  */
/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode --param=analyzer-text-art-string-ellipsis-threshold=0" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

#include <string.h>

void
test_non_ascii ()
{
  char buf[9];
  strcpy (buf, "Liberté\n"); /* { dg-warning "stack-based buffer overflow" } */
  /* { dg-warning "'__builtin_memcpy' writing 10 bytes into a region of size 9 overflows the destination" "" { target *-*-* } .-1 } */
}

/* { dg-begin-multiline-output "" }

  ┌──────┬──────┬──────┬──────┬──────┬──────┬────┬────┬──────┐┌─────────────────┐
  │ [0]  │ [1]  │ [2]  │ [3]  │ [4]  │ [5]  │[6] │[7] │ [8]  ││       [9]       │
  ├──────┼──────┼──────┼──────┼──────┼──────┼────┼────┼──────┤├─────────────────┤
  │ 0x4c │ 0x69 │ 0x62 │ 0x65 │ 0x72 │ 0x74 │0xc3│0xa9│ 0x0a ││      0x00       │
  ├──────┼──────┼──────┼──────┼──────┼──────┼────┴────┼──────┤├─────────────────┤
  │U+004c│U+0069│U+0062│U+0065│U+0072│U+0074│ U+00e9  │U+000a││     U+0000      │
  ├──────┼──────┼──────┼──────┼──────┼──────┼─────────┼──────┤├─────────────────┤
  │  L   │  i   │  b   │  e   │  r   │  t   │    é    │      ││       NUL       │
  ├──────┴──────┴──────┴──────┴──────┴──────┴─────────┴──────┴┴─────────────────┤
  │                      string literal (type: 'char[10]')                      │
  └─────────────────────────────────────────────────────────────────────────────┘
     │      │      │      │      │      │     │    │     │             │
     │      │      │      │      │      │     │    │     │             │
     v      v      v      v      v      v     v    v     v             v
  ┌──────┬────────────────────────────────────────────┬──────┐┌─────────────────┐
  │ [0]  │                    ...                     │ [8]  ││                 │
  ├──────┴────────────────────────────────────────────┴──────┤│after valid range│
  │                 'buf' (type: 'char[9]')                  ││                 │
  └──────────────────────────────────────────────────────────┘└─────────────────┘
  ├────────────────────────────┬─────────────────────────────┤├────────┬────────┤
                               │                                       │
                      ╭────────┴────────╮                    ╭─────────┴────────╮
                      │capacity: 9 bytes│                    │overflow of 1 byte│
                      ╰─────────────────╯                    ╰──────────────────╯

   { dg-end-multiline-output "" } */
