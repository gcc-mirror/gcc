/* { dg-options "-O3 -fdump-tree-cunrolli-details" } */

static int s[10][10][10];
static int d[10][10][10];

__attribute__((noipa)) 
int array()
{
	int i;
	register int j, k;
	for (i = 0; i < 10; i++)
		for (j = 0; j < 10; j++)
			for (k = 0; k < 10; k++)
				d[i][j][k] = s[i][j][k];

	return(0);
}

__attribute__((noipa)) 
void TestBench()
{
	for (int i = 0; i < 150000; ++i)
	{ 
	   array();
	}
}

int main(int argc, char *argv[])
{

	TestBench();

	if (d[9][9][9] == 0 && s[9][9][9] == 0)
	{
		return 0;
	}
	else
	{
		return -1;
	}	
}

/* { dg-final-use { scan-tree-dump-times "loop with 10 iterations completely unrolled" 2 "cunrolli"} } */
