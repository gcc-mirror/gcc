unsigned short int showbug(unsigned short int *a, unsigned short int *b)
{
        *a += *b -8;
        return (*a >= 8);
}

int main()
{
        unsigned short int x = 0;
        unsigned short int y = 10;

        if (showbug(&x, &y) != 0)
	  abort (0);

	exit (0);
}

