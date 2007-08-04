typedef __SIZE_TYPE__ size_t;
extern void dont_optimize_away(size_t);

void crashGcc(char*a)
{
        size_t b=(size_t)a - ((size_t)a & 1);
        size_t c=(size_t)a - (b & (size_t)a);
        dont_optimize_away(b+c);
}

