typedef struct s {
	unsigned short a;
	unsigned long b __attribute__ ((packed));
} s;

s t;

int main()
{
        t.b = 0;
	return 0;
}
