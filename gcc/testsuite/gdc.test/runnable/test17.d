
import core.stdc.stdio: fflush, stdout;

extern(C) int printf(const char*, ...);

void ulog(string s)
{
    printf("%.*s\n",cast(int)s.length, s.ptr);
    fflush(stdout);
}

int open()
{
    char *s;
    char[2000] abs;
    char[100] qu;
    int a;
    ulog("reaches this only 9 times of 10!\n");
    return 0;
}


int yhenda()
{
    char[2200] MEM;
    int a;
    ulog("point(2.1) \n");
    open();
    ulog("point(2.2) \n");
    return 0;
}


int main()
{
    printf("Content-type: text/html\n\n");
    fflush(stdout);
    ulog("point(1.1)\n");
    yhenda();
    ulog("point(1.2)\n");
    return 0;
}
