foo(char *bufp)
{
    int x = 80;
    return (*bufp++ = x ? 'a' : 'b');
}
