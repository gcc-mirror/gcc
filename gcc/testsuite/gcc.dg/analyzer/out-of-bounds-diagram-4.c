/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

#include <string.h>

#define LOREM_IPSUM \
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod" \
  " tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim" \
  " veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea" \
  " commodo consequat. Duis aute irure dolor in reprehenderit in voluptate" \
  " velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint" \
  " occaecat cupidatat non proident, sunt in culpa qui officia deserunt" \
  " mollit anim id est laborum."

void
test_long_string ()
{
  char buf[100];
  strcpy (buf, LOREM_IPSUM); /* { dg-warning "stack-based buffer overflow" } */
  /* { dg-warning "'__builtin_memcpy' writing 446 bytes into a region of size 100 overflows the destination" "" { target *-*-* } .-1 } */
}

/* { dg-begin-multiline-output "" }

  ┌───┬───┬───┬───┬───┬───┬──────────┬─────┬─────┬─────┬─────┬─────┬─────┐
  │[0]│[1]│[2]│[3]│[4]│[5]│          │[440]│[441]│[442]│[443]│[444]│[445]│
  ├───┼───┼───┼───┼───┼───┤   ...    ├─────┼─────┼─────┼─────┼─────┼─────┤
  │'L'│'o'│'r'│'e'│'m'│' '│          │ 'o' │ 'r' │ 'u' │ 'm' │ '.' │ NUL │
  ├───┴───┴───┴───┴───┴───┴──────────┴─────┴─────┴─────┴─────┴─────┴─────┤
  │                  string literal (type: 'char[446]')                  │
  └──────────────────────────────────────────────────────────────────────┘
    │   │   │   │   │   │  │  │    │    │     │     │     │     │     │
    │   │   │   │   │   │  │  │    │    │     │     │     │     │     │
    v   v   v   v   v   v  v  v    v    v     v     v     v     v     v
  ┌───┬─────────────────────┬────┐┌──────────────────────────────────────┐
  │[0]│         ...         │[99]││                                      │
  ├───┴─────────────────────┴────┤│          after valid range           │
  │  'buf' (type: 'char[100]')   ││                                      │
  └──────────────────────────────┘└──────────────────────────────────────┘
  ├──────────────┬───────────────┤├──────────────────┬───────────────────┤
                 │                                   │
       ╭─────────┴─────────╮              ╭──────────┴──────────╮
       │capacity: 100 bytes│              │overflow of 346 bytes│
       ╰───────────────────╯              ╰─────────────────────╯

   { dg-end-multiline-output "" } */
