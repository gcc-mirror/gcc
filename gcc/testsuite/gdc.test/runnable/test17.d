
import core.stdc.stdio: fflush, stdout;

extern(C) int printf(const char*, ...);

void ulog(string s)
{
    printf("%.*s\n",s.length, s.ptr);
    fflush(stdout);
}

int open()
{
    char *s;
    char abs[2000];
    char qu[100];
    int a;
    ulog("reaches this only 9 times of 10!\n");
    return 0;
}


int yhenda()
{
    char MEM[2200];
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

