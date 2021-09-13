/* This test assumes -fbuiltin and not -fansi to get "index" and "memchr" builtin DECLs.  */
/* { dg-do compile } */
/* { dg-options "" } */

/* The bug this is testing is that if a new decl conflicts with an
   explicit decl, you don't get the "changes type of builtin" message,
   but if there was *also* a builtin, you *also* don't get the "previous
   declaration" message, leaving you with no clue where the previous
   declaration came from.  */

extern char foo(int,int); /* { dg-message "previous declaration of 'foo'" "note" } */
extern char *index(const char *,int); /* { dg-message "previous declaration of 'index'" "note" } */

/* This changes the type of "index", which is both a builtin and an
   explicit decl.  */
int index; /* { dg-error "redeclared as different kind of symbol" } */

/* This changes the type of "memchr", which is only a builtin.  */
int memchr; /* { dg-warning "built-in function 'memchr' declared as non-function" } */

/* This changes the type of "foo", which is only an explicit decl.  */
int foo; /* { dg-error "redeclared as different kind of symbol" } */
