/* PR middle-end/13325 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);
void f (void *dest, const void *src) {
    memcpy (dest, src, 0);
}
