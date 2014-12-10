/* PR c/56980 */
/* { dg-do compile } */

typedef struct A { int i; } B;
typedef union U { int i; } V;
typedef enum E { G } F;

void foo_s (struct A); /* { dg-message "expected .struct A. but argument is of type .B \\* {aka struct A \\*}." } */
void foo_u (union U); /* { dg-message "expected .union U. but argument is of type .V \\* {aka union U \\*}." } */
void foo_e (enum E); /* { dg-message "expected .enum E. but argument is of type .F \\* {aka enum E \\*}." } */
void foo_sp (B *); /* { dg-message "expected .B \\* {aka struct A \\*}. but argument is of type .struct B \\*." } */
void foo_up (V *); /* { dg-message "expected .V \\* {aka union U \\*}. but argument is of type .union V \\*." } */
void foo_ep (F *); /* { dg-message "expected .F \\* {aka enum E \\*}. but argument is of type .enum F \\*." } */

void 
bar (B *b, V *v, F *f)
{
  foo_s (b); /* { dg-error "incompatible" } */
  foo_u (v); /* { dg-error "incompatible" } */
  foo_e (f); /* { dg-error "incompatible" } */
  foo_sp ((struct B *) b); /* { dg-error "passing argument" } */
  foo_up ((union V *) v); /* { dg-error "passing argument" } */
  foo_ep (__extension__ (enum F *) f); /* { dg-error "passing argument" } */
}
