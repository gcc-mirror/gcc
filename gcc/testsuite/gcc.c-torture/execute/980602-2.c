/* The bit-field below would have a problem if __INT_MAX__ is too
   small.  */
#if __INT_MAX__ < 2147483647
int
main (void)
{
  exit (0);
}
#else
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
#endif
