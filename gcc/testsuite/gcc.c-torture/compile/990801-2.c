void f()
{
    extern char* p;
    int ch;
    while (!(ch = 0)) {
        if ((ch == 0) || (ch == 2)) {
            break;
        }
        *p = 0;
    }
}
