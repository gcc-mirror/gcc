/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <string.h>
#include <stdlib.h>
#include <stdint.h>

void test8 (size_t size, size_t offset)
{
  char src[size];
  char dst[size];
  memcpy (dst, src, size + offset); /* { dg-line test8 } */
  /* { dg-warning "over-read" "warning" { target *-*-* } test8 } */
  /* { dg-warning "overflow" "warning" { target *-*-* } test8 } */
}

/* { dg-begin-multiline-output "" }

  ┌──────────────────────────────────────────────────────────────────────┐
  │                    read of 'size + offset' bytes                     │
  └──────────────────────────────────────────────────────────────────────┘
                   ^                                   ^
                   │                                   │
                   │                                   │
  ┌──────────────────────────────────┐┌──────────────────────────────────┐
  │ buffer allocated on stack at (1) ││        after valid range         │
  └──────────────────────────────────┘└──────────────────────────────────┘
  ├────────────────┬─────────────────┤├────────────────┬─────────────────┤
                   │                                   │
         ╭─────────┴────────╮            ╭─────────────┴─────────────╮
         │size: 'size' bytes│            │over-read of 'offset' bytes│
         ╰──────────────────╯            ╰───────────────────────────╯

   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }

  ┌──────────────────────────────────────────────────────────────────────┐
  │                    write of 'size + offset' bytes                    │
  └──────────────────────────────────────────────────────────────────────┘
                   │                                   │
                   │                                   │
                   v                                   v
  ┌──────────────────────────────────┐┌──────────────────────────────────┐
  │ buffer allocated on stack at (1) ││        after valid range         │
  └──────────────────────────────────┘└──────────────────────────────────┘
  ├────────────────┬─────────────────┤├────────────────┬─────────────────┤
                   │                                   │
       ╭───────────┴──────────╮          ╭─────────────┴────────────╮
       │capacity: 'size' bytes│          │overflow of 'offset' bytes│
       ╰──────────────────────╯          ╰──────────────────────────╯

   { dg-end-multiline-output "" } */
