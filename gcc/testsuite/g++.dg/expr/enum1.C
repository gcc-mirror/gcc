// { dg-do run }

extern "C" void abort();
int main()
{
    enum { shelf = 4 } t = shelf;
    if (!(t & shelf))
	abort ();
}

