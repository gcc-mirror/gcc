// { dg-do run }

void abort();
int main()
{
    enum { shelf = 4 } t = shelf;
    if (!(t & shelf))
	abort ();
}

