// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=90601
// { dg-do compile }

int postincr(int a)
{
    return (a += 1.0)++;
}

int postdecr(int a)
{
    return (a -= 1.0)--;
}

int preincr(int a)
{
    return ++(a += 1.0);
}

int predecr(int a)
{
    return --(a -= 1.0);
}
