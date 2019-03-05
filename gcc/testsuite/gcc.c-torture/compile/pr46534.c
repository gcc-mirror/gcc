/* { dg-skip-if "too big" { nvptx-*-* } } */
/* PR middle-end/46534 */

extern int printf (const char *, ...);

#define S1 "                    "
#define S2 S1 S1 S1 S1 S1 S1 S1 S1 S1 S1
#define S3 S2 S2 S2 S2 S2 S2 S2 S2 S2 S2
#define S4 S3 S3 S3 S3 S3 S3 S3 S3 S3 S3
#define S5 S4 S4 S4 S4 S4 S4 S4 S4 S4 S4
#define S6 S5 S5 S5 S5 S5 S5 S5 S5 S5 S5
#define S7 S6 S6 S6 S6 S6 S6 S6 S6 S6 S6

void
foo (void)
{
  printf (S7 "\n");  /* { dg-error "size of string literal is too large" "" { target { ! size32plus } } } */
}
