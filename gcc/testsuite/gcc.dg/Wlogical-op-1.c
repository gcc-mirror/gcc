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
    if ( testa() && b )     /* { dg-warning "logical" } */
         (void)testa();

    if ( c && b )           /* { dg-warning "logical" } */
	(void)testa();

    if ( c && 0x42 )        /* { dg-warning "logical" } */
	(void)testa();

    if ( c && 0x80 >>6)     /* { dg-warning "logical" } */
	(void)testa();


    if ( b && c == a )      /* { dg-bogus "logical" } */
          (void)testa();

    if ( 1 && c )           /* { dg-bogus "logical" } */
         (void)testa();

    if ( t2 && b )          /* { dg-bogus "logical" } */
          (void)testa();

    if ( 0 && c == a )      /* { dg-bogus "logical" } */
          (void)testa();

    if ( b && 1 )           /* { dg-bogus "logical" } */
          (void)testa();
}


void bar()
{
    if ( testa() || b )     /* { dg-warning "logical" } */
         (void)testa();

    if ( c || b )           /* { dg-warning "logical" } */
	(void)testa();

    if ( c || 0x42 )        /* { dg-warning "logical" } */
	(void) testa();

    if ( c || 0x80 >>6)     /* { dg-warning "logical" } */
	(void)testa();


    if ( b || c == a )      /* { dg-bogus "logical" } */
          (void)testa();

    if ( 1 || c )           /* { dg-bogus "logical" } */
         (void)testa();

    if ( t2 || b )          /* { dg-bogus "logical" } */
          (void)testa();

    if ( 0 || c == a )      /* { dg-bogus "logical" } */
          (void)testa();

    if ( b || 1 )           /* { dg-bogus "logical" } */
          (void)testa();
}

