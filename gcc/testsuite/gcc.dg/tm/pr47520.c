/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

struct ReadSeqVars
{
	int format;
	char *ss;
};

void rms_feof(struct ReadSeqVars *);

__attribute__((transaction_callable)) int ReadSeq(struct ReadSeqVars *V)
{
	if (V->format > 1)
	{
		if ((V->format != 2) && (V->ss != (void*)0) )
		{
			V->format = 3;
		}
	}
	else
	{
		int i = 0;
		for (i = 0; i < 1; i++)
		{
		}
		rms_feof(V);
	}
}
