/* PR 16684: no redundant declaration warnings should issue the first
   time a built-in function is declared.
   { dg-do compile }
   { dg-options "-Wredundant-decls" } */

void *malloc (__SIZE_TYPE__);  /* { dg-bogus "redundant" } */
void *malloc (__SIZE_TYPE__);  /* { dg-warning "redundant" } */
