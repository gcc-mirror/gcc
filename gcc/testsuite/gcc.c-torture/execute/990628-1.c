#define TP_BSIZE 64

char	buf[2*TP_BSIZE];
char	(*nextblock)[TP_BSIZE] = (char (*)[TP_BSIZE]) buf;

union u_test {
	char dummy[TP_BSIZE];
	struct s_test {
		int a;
		int b;
		int c;
	} s_test;
};

main(int argc, char **argv)
{
	int i;
	char dp[TP_BSIZE];

	for (i = 0; i < 2*TP_BSIZE; i++)
		buf[i] = '.';
	for (i = 0; i < TP_BSIZE; i++)
		dp[i] = 'a';

	*(union u_test *)(*(nextblock)++) = *(union u_test *)dp;

	for (i = 0; i < 2*TP_BSIZE; i++)
		printf("%c%s", buf[i], (i % 64) == 63 ? "\n" : "");
	exit(0);
}
