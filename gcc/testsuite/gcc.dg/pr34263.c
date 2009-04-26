/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* Same test as 990128-1.c.  */

extern int printf (const char *,...);
extern void abort (void);

struct s { struct s *n; } *p;
struct s ss;
#define MAX     10
struct s sss[MAX];
int count = 0;

void sub( struct s *p, struct s **pp );
int look( struct s *p, struct s **pp );

main()
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
      abort ();

    return( 0 );
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

int look( struct s *p, struct s **pp )
{
    for ( ; p; p = p->n )
        ;
    *pp = p;
    count++;
    return( 1 );
}

/* { dg-final { scan-tree-dump "Cleaned-up latch block of loop with single BB" "optimized" { xfail { *-*-* } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

