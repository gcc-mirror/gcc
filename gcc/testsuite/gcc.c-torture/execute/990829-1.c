double test (const double le, const double ri)
{
	double val = ( ri - le ) / ( ri * ( le + 1.0 ) );
	return val;
}

int main ()
{
        double retval;

	retval = test(1.0,2.0);
        if (retval < 0.24 || retval > 0.26)
	  abort ();
	exit (0);
}
