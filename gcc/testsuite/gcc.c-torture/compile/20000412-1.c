typedef struct {
        short   a;
        short   b;
} s1;

extern void g(unsigned char *b);

void f(void)
{
        s1        a;
	unsigned char *b;

        a.a = 0;
	b = (unsigned char *)&a;	
        g(b);           
}
