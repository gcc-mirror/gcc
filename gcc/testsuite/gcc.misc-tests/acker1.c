int acker(int, int);

void main()
{
    acker(3,6);
    /* */
    exit(0);
    /* */
}

int
acker(int x,int y)
{
    if (x==0)
	return(y+1);
    else if (y==0)
	return(acker(x-1,1));
    else
	return(acker(x-1, acker(x, y-1)));
}
