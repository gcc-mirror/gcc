typedef struct {
    int x;
} FILE;
extern void fputs (const char *, FILE *);

int mView;
void foo (FILE * out, int aIndent)
{
    if (0 != mView) {
	aIndent++;
	aIndent--;
	{
	    int __t = aIndent;
	    while (--__t >= 0)
		fputs ("  ", out);
	}

    } {
	int __t = aIndent;
	while (--__t >= 0)
	    fputs ("  ", out);
    }
}
