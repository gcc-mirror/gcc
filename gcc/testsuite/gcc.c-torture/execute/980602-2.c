struct {
    unsigned bit : 30;
} t;

int main()
{
    if (!(t.bit++))
	exit (0);
    else
	abort ();
}
