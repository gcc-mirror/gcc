/* PR optimization/13472 */
/* Origin: <p.van-hoof@qub.ac.uk> */

/* Verify that the reload pass doesn't emit a store
   to the .rodata section. */

/* { dg-do run } */
/* { dg-options "-O2 -pedantic -march=i686" { target i686-*-* } } */


#define MAX2(a,b) (((a)>(b)) ? (a) : (b))

const int q=0, p=0;

typedef struct {
	float a;
	float b;
} F;

F **E, *D, C = { 2.f, 1.f };

void G(float);
void H(void);

int main(void)
{
	D = &C;
	E = &D;

	H();
        return 0;
}

void H(void)
{
	int i, l=1;
	float b, o;

	if( l )
	{
		b = 0.3f * MAX2(0.f,E[q][p].a - E[q][p].b);
		o = E[q][p].a;
		if( o > 1.e-36f )
                        G(o);
		E[q][p].a *= b;
	}
	else
		b = 1.f;
	for( i=q; i<2; ++i )
		;
}

void G(float o)
{
}

