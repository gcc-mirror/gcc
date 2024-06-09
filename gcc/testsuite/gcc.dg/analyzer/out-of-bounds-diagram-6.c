/* { dg-require-effective-target lp64 }
   Misbehaves with -m32 due to optimization turning the pointer arithmetic into:
      _2 = &buf + 4294967246;
      memcpy (_2, _1, 4096);
*/

/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <string.h>

#define LOREM_IPSUM \
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod" \
  " tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim" \
  " veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea" \
  " commodo consequat. Duis aute irure dolor in reprehenderit in voluptate" \
  " velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint" \
  " occaecat cupidatat non proident, sunt in culpa qui officia deserunt" \
  " mollit anim id est laborum."

/* This memcpy reads from both before and after the bounds of the
   string literal, and writes to both before and after the bounds of "buf".  */

void
test_bad_memcpy ()
{
  char buf[100];
  memcpy (buf - 50, LOREM_IPSUM - 100, 4096); /* { dg-warning "stack-based buffer overflow" } */
  /* { dg-warning "stack-based buffer underwrite" "" { target *-*-* } .-1 } */
  /* { dg-warning "buffer under-read" "" { target *-*-* } .-2 } */
  /* { dg-warning "buffer over-read" "" { target *-*-* } .-3 } */
  /* { dg-warning "'memcpy' writing 4096 bytes into a region of size 0 overflows the destination" "" { target *-*-* } .-4 } */
}

/* { dg-begin-multiline-output "" }

  ┌────────────────────────────────────────────────────────────────────────────────────────────┐
  │                                     read of 4096 bytes                                     │
  └────────────────────────────────────────────────────────────────────────────────────────────┘
           ^            ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^    ^            ^
           │            │   │   │   │   │   │   │   │   │   │   │   │    │            │
           │            │   │   │   │   │   │   │   │   │   │   │   │    │            │
  ┌──────────────────┐┌───┬───────────────────────────────────────────┬─────┐┌─────────────────┐
  │                  ││[0]│                    ...                    │[445]││                 │
  │                  │├───┼───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┼─────┤│                 │
  │before valid range││'L'│'o'│'r'│'e'│'m'│' '│...│'o'│'r'│'u'│'m'│'.'│ NUL ││after valid range│
  │                  │├───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴─────┤│                 │
  │                  ││         string literal (type: 'char[446]')          ││                 │
  └──────────────────┘└─────────────────────────────────────────────────────┘└─────────────────┘
  ├────────┬─────────┤├──────────────────────────┬──────────────────────────┤├────────┬────────┤
           │                                     │                                    │
  ╭────────┴──────────────╮              ╭───────┴───────╮                ╭───────────┴───────────╮
  │under-read of 100 bytes│              │size: 446 bytes│                │over-read of 3550 bytes│
  ╰───────────────────────╯              ╰───────────────╯                ╰───────────────────────╯

   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }

  ┌──────────────────────────────────────────────────────────────────────┐
  │                         write of 4096 bytes                          │
  └──────────────────────────────────────────────────────────────────────┘
           │              │       │        │                 │
           │              │       │        │                 │
           v              v       v        v                 v
  ┌──────────────────┐┌───────┬───────┬─────────┐┌───────────────────────┐
  │                  ││  [0]  │  ...  │  [99]   ││                       │
  │before valid range│├───────┴───────┴─────────┤│   after valid range   │
  │                  ││'buf' (type: 'char[100]')││                       │
  └──────────────────┘└─────────────────────────┘└───────────────────────┘
  ├────────┬─────────┤├────────────┬────────────┤├───────────┬───────────┤
           │                       │                         │
           │             ╭─────────┴─────────╮   ╭───────────┴──────────╮
           │             │capacity: 100 bytes│   │overflow of 3946 bytes│
           │             ╰───────────────────╯   ╰──────────────────────╯
  ╭────────┴─────────────╮
  │underwrite of 50 bytes│
  ╰──────────────────────╯

   { dg-end-multiline-output "" } */

/* The read and write diagrams are each emitted twice: once for the "before"
   and once for the "after" diagnostic.  */

/* { dg-begin-multiline-output "" }

  ┌────────────────────────────────────────────────────────────────────────────────────────────┐
  │                                     read of 4096 bytes                                     │
  └────────────────────────────────────────────────────────────────────────────────────────────┘
           ^            ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^    ^            ^
           │            │   │   │   │   │   │   │   │   │   │   │   │    │            │
           │            │   │   │   │   │   │   │   │   │   │   │   │    │            │
  ┌──────────────────┐┌───┬───────────────────────────────────────────┬─────┐┌─────────────────┐
  │                  ││[0]│                    ...                    │[445]││                 │
  │                  │├───┼───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┼─────┤│                 │
  │before valid range││'L'│'o'│'r'│'e'│'m'│' '│...│'o'│'r'│'u'│'m'│'.'│ NUL ││after valid range│
  │                  │├───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴─────┤│                 │
  │                  ││         string literal (type: 'char[446]')          ││                 │
  └──────────────────┘└─────────────────────────────────────────────────────┘└─────────────────┘
  ├────────┬─────────┤├──────────────────────────┬──────────────────────────┤├────────┬────────┤
           │                                     │                                    │
  ╭────────┴──────────────╮              ╭───────┴───────╮                ╭───────────┴───────────╮
  │under-read of 100 bytes│              │size: 446 bytes│                │over-read of 3550 bytes│
  ╰───────────────────────╯              ╰───────────────╯                ╰───────────────────────╯

   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }

  ┌──────────────────────────────────────────────────────────────────────┐
  │                         write of 4096 bytes                          │
  └──────────────────────────────────────────────────────────────────────┘
           │              │       │        │                 │
           │              │       │        │                 │
           v              v       v        v                 v
  ┌──────────────────┐┌───────┬───────┬─────────┐┌───────────────────────┐
  │                  ││  [0]  │  ...  │  [99]   ││                       │
  │before valid range│├───────┴───────┴─────────┤│   after valid range   │
  │                  ││'buf' (type: 'char[100]')││                       │
  └──────────────────┘└─────────────────────────┘└───────────────────────┘
  ├────────┬─────────┤├────────────┬────────────┤├───────────┬───────────┤
           │                       │                         │
           │             ╭─────────┴─────────╮   ╭───────────┴──────────╮
           │             │capacity: 100 bytes│   │overflow of 3946 bytes│
           │             ╰───────────────────╯   ╰──────────────────────╯
  ╭────────┴─────────────╮
  │underwrite of 50 bytes│
  ╰──────────────────────╯

   { dg-end-multiline-output "" } */
