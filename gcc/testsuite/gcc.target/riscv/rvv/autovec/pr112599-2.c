/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl1024b -mabi=lp64d -O3" } */

struct s { struct s *n; } *p;
struct s ss;
#define MAX     10
struct s sss[MAX];
int count = 0;

int look( struct s *p, struct s **pp )
{
    for ( ; p; p = p->n )
        ;
    *pp = p;
    count++;
    return( 1 );
}

void sub( struct s *p, struct s **pp )
{
   for ( ; look( p, pp ); ) {
        if ( p )
            p = p->n;
        else
            break;
   }
}

int
foo(void)
{
    struct s *pp;
    struct s *next;
    int i;

    p = &ss;
    next = p;
    for ( i = 0; i < MAX; i++ ) {
        next->n = &sss[i];
        next = next->n;
    }
    next->n = 0;

    sub( p, &pp );
    if (count != MAX+2)
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-not {vrgather} } } */
/* { dg-final { scan-assembler {vslide} } } */
