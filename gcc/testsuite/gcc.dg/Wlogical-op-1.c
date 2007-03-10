/*
   { dg-do compile}
   { dg-options "-Wlogical-op" }
*/

enum { a, ba, b };

enum testenum { t1, t2};

extern int c;
extern char bool_a, bool_b;

extern int testa();

void foo()
{
    if ( testa() && b )     /* { dg-warning "always evaluate as" } */
         (void)testa();

    if ( c && b )           /* { dg-warning "always evaluate as" } */
	(void)testa();

    if ( c && 0x42 )        /* { dg-warning "always evaluate as" } */
	(void)testa();

    if ( c && 0x42 )        /* { dg-warning "always evaluate as" } */
	(void) testa();

    if ( c && 0x80 >>6)     /* { dg-warning "always evaluate as" } */
	(void)testa();


    if ( b && c == a )      /* { dg-bogus "always evaluate as" } */
          (void)testa();

    if ( 1 && c )           /* { dg-bogus "always evaluate as" } */
         (void)testa();

    if ( t2 && b )          /* { dg-bogus "always evaluate as" } */
          (void)testa();

    if ( 0 && c == a )      /* { dg-bogus "always evaluate as" } */
          (void)testa();

    if ( b && 1 )           /* { dg-bogus "always evaluate as" } */
          (void)testa();
}
