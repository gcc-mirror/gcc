/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */

typedef struct
{
	char	c[510];
} s510;

typedef struct
{
	char	c[511];
} s511;

s510	G510, s1;
s511	G511;
int	I, J;
double	D;

void main(void);
void f0(double D, ...);
s510 f1(double D, ...);
void f2a(s510 S);
void f2b(s511 S);


void main(void)
{

	f0(D, I, J);

	s1 = f1(D, I, D);

	f2a(G510);

	f2b(G511);

}
