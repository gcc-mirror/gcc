/* { dg-do run { target rs6000-*-linux* powerpc*-*-linux*} } */
/* { dg-options "-O2 -fpic" } */

extern void exit (int);

void foo1(int a, char *b, int c)
{
   c =a+c+234;
}

int foo2(int d)
{
   return d*d;
}

int bar1, bar2, bar3;
char * bar4;

int main(void) {
    int h;
    bar1 = foo2(1);
    bar2 = foo2(1);

    h = foo2(1);
    foo1(1, "a", foo2(1));
    foo1(bar1, "a", foo2(1));
    foo2(1);

    h = foo2(1);
    bar3 = 1;
    bar4 = "a";
    foo1(1, "n", foo2(1));
    foo1(1, "o", foo2(1));
    foo1(1, "p", foo2(1));
    foo1(bar1, "a", foo2(1));

    bar3 = h;
    bar4 = "b";  foo1(bar1, "b", foo2(1));
    foo1(1, "q", foo2(1));
    bar4 = "c";  foo1(1, "c", foo2(1));
    bar4 = "d";  foo1(1, "d", foo2(1));
    bar4 = "e";  foo1(1, "e", foo2(1));
    bar4 = "f";  foo1(1, "f", foo2(1));
    bar4 = "g";  foo1(1, "g", foo2(1));
    bar4 = "h";  foo1(1, "h", foo2(1));
    bar4 = "i";  foo1(1, "i", foo2(1));
    bar4 = "j";  foo1(1, "j", foo2(1));
    bar4 = "k";  foo1(1, "k", foo2(1));
    bar4 = "l";  foo1(1, "l", foo2(1));
    bar4 = "m";
    foo1(bar2, "m", foo2(1));
    exit(0);
}
