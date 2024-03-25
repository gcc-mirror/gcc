/* { dg-do compile } */
/* PR c/100532 */

typedef __SIZE_TYPE__ size_t;
void *memcpy(void[], const void *, size_t); /* { dg-error "declaration of type name" } */
void c(void) { memcpy(c, "a", 2); } /* { dg-error "type of formal parameter" } */

