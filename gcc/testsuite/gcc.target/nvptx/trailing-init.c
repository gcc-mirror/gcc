/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic" } */

struct trailing 
{
  unsigned m;
  short ary[];
} trailing = 
  {.ary = {1}};

struct packed 
{
  unsigned m;
  short ary[];
} __attribute__ ((packed)) packed = 
  {.ary = {2}};

/*  { dg-final { scan-assembler ".align 1 .u32 packed\\\[2\\\] = { 0, 2 };" } } */
/*  { dg-final { scan-assembler ".align 4 .u32 trailing\\\[2\\\] = { 0, 1 };" } } */
