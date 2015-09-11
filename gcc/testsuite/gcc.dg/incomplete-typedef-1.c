/* { dg-do compile } */
/* { dg-options "" } */

typedef struct S TS;
typedef union U TU;

void
foo (void)
{
  (TS) { }; /* { dg-error "invalid use of incomplete typedef" } */
  (TU) { }; /* { dg-error "invalid use of incomplete typedef" } */
}
