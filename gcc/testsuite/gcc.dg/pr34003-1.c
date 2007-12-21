/* PR bootstrap/34003 */
/* { dg-do link } */
/* { dg-options "-O0" } */
/* { dg-additional-sources "pr34003-2.c" } */

extern void foo (void);
int bar (void) { foo (); return 1; }
extern void foo (void);
