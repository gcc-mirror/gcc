/* PR c/50584 - No warning for passing small array to C99 static array
   declarator

   Verify that out-of-bounds accesses to array arguments are diagnosed,
   both to ordinary array parameters with constant bounds and to array
   parameters declared static.  This is the converse of what PR 50584
   asks for.

   { dg-do compile }
   { dg-options "-O2 -Wall -Warray-parameter -Wno-vla-parameter" } */

#define NOIPA __attribute__  ((noipa))

void sink (void*, ...);

#define T(...) sink (0, __VA_ARGS__)


NOIPA void fca1 (char a[1])
{
  T (a[0]);
  T (a[1]);                   // { dg-warning "-Warray-bounds" }
}

NOIPA void fcas1 (char a[static 1])
{
  T (a[0]);
  T (a[1]);                   // { dg-warning "-Warray-bounds" }
}

NOIPA void fca2 (char a[2])
{
  T (a[0]); T (a[1]);
  T (a[2]);                   // { dg-warning "-Warray-bounds" }
}

NOIPA void fcas2 (char a[static 2])
{
  T (a[0]); T (a[1]);
  T (a[2]);                   // { dg-warning "-Warray-bounds" }
}

NOIPA void fca3 (char a[3])
{
  T (a[0]); T (a[1]); T (a[2]);
  T (a[3]);                   // { dg-warning "-Warray-bounds" }
}

NOIPA void fcas3 (char a[static 3])
{
  T (a[0]); T (a[1]); T (a[2]);
  T (a[3]);                   // { dg-warning "-Warray-bounds" }
}


NOIPA void fca1_1 (char a[1][1])
{
  T (a[0][0]);
  T (a[0][1]);                // { dg-warning "-Warray-bounds" }
}
