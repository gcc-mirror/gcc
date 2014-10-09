/* PR c/62024 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11 -Wpedantic" } */
/* { dg-require-effective-target sync_char_short } */

int *p;
_Static_assert (__atomic_always_lock_free (1, p), ""); /* { dg-warning "is not an integer constant" } */
_Static_assert (__atomic_always_lock_free (1, 0), ""); /* { dg-warning "is not an integer constant" } */
