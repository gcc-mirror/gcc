/* { dg-do run } */

void abort (void);
void exit (int);

void foo (int i)
{
    static int n;
    if (i < -128 || i > 127)
        abort ();
    if (++n > 1000)
        exit (0);
}

int main ()
{
    signed char c;
    for (c = 0; ; c++) foo (c);
}
