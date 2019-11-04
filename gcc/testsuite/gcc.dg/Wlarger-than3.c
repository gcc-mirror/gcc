/* Exercise -Wlarger-than= with a byte-size suffix.
   { dg-do compile }
   { dg-skip-if "small address space" { "pdp11-*-*" } { "avr-*-*" } }
   { dg-options "-Wlarger-than=1MiB" } */

#define MB (1000 * 1000)    /* MegaByte */
#define MiB (1024 * 1024)   /* MebiByte */

char megabyte[MB];
char membibyte[MiB];

char megabyte_plus_1[MB + 1];
char membibyte_plus_1[MiB + 1];   /* { dg-warning "size of .membibyte_plus_1. 1048577 bytes exceeds maximum object size 1048576" } */
