// PR 10352
// { dg-do compile }
// { dg-options -O2 }

extern double fabs(double x);

typedef struct { float x, y; } S;
typedef struct _T T;

extern void fT( T *p );
extern T *h();
extern S g( );

static
int f(void)
{
    T *t=0;
    int c=0;
    S s;

    const S exp_s = {0.,0.};

    if(!(t = h()))
    {
        c++;
    }

    if(!c)
    {
        s = g();
        if( (fabs( (s.x) - (exp_s.x) ) > 1 )
            || (fabs( (s.y) - (exp_s.y) ) > 1 ) )
        {
            c++;
        }
    }

    if(t)
        fT(t);

    return c;
}
