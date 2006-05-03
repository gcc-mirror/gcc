typedef union {
    int d;
} U;

int rv;
void breakme()
{
    U *rv0;
    U *pretmp = (U*)&rv;
    rv0 = pretmp;
    rv0->d = 42;
}

