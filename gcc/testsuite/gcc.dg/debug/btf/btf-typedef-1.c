/* Test BTF generation for BTF_KIND_TYPEDEF records.

   7 typedef records are expected. We expect the following types (among others):
     [1] int 'int' size=4 offset=0 bits=32 SIGNED
     [2] typedef 'my_int' type=1
     [3] typedef 'foo_int' type=1
     [4] typedef 'bar_int' type=1
     ..
     [6] typedef 'CBAR' type=5
     ..
     [8] typedef 'CBARP' type=7
     [9] struct '_node' size=16
     ..
     [11] typedef 'node_t' type=9
     [12] struct '_arena'
     ..
     [15] typedef 'arena_t' type=12
     [16] var 'a' type=2 linkage=1 (global)
     [17] var 'suitcase' type=15 linkage=1 (global)
     [18] var 'b' type=3 linkage=1 (global)
     [19] var 'c' type=4 linkage=1 (global)
     [20] var 'd' type=11 linkage=1 (global)
     [21] var 'destination' type=6 linkage=1 (global)
     [22] var 'ticket' type=8 linkage=1 (global)

   Note that the order of the variables is not guaranteed, so we do not check
   particular variables have exactly the right typedef. Instead, we check:
   1. 7 typedef records are generated, along with the correct strings for them.
   2. There is one variable pointing to each typedef.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x8000000\[\t \]+\[^\n\]*btt_info" 7 } } */

/* { dg-final { scan-assembler-times "ascii \"my_int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo_int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"bar_int.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"CBAR.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"CBARP.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"node_t.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"arena_t.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*btv_type" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x3\[\t \]+\[^\n\]*btv_type" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x4\[\t \]+\[^\n\]*btv_type" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x6\[\t \]+\[^\n\]*btv_type" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x8\[\t \]+\[^\n\]*btv_type" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xb\[\t \]+\[^\n\]*btv_type" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xf\[\t \]+\[^\n\]*btv_type" 1 } } */

typedef int my_int;
typedef int foo_int;
typedef int bar_int;

typedef const bar_int CBAR;
typedef const bar_int * CBARP;

typedef struct _node
{
  foo_int name_off;
  bar_int info;
  struct _node * next;
} node_t;


typedef struct _arena
{
  node_t nodes[16];
  my_int vardata;
  bar_int flags;
} arena_t;

my_int a;
foo_int b;
bar_int c;
node_t d;

CBAR destination;
CBARP ticket = &destination;

arena_t suitcase;
