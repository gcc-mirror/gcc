/* { dg-do compile } */
/* { dg-options "-Wall" } */

static void asm_fn(); /* { dg-bogus "but never defined" } */
asm("%cc0:" :: ":"(&asm_fn));
