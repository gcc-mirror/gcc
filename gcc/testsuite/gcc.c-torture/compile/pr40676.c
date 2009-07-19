extern int f1();
extern int f2(void*);
extern void* f3(int);
int xmsih;
typedef unsigned short XWCHAR;

inline unsigned int xstrlenW( const XWCHAR *str )
{
    const XWCHAR *s = str;
    while (*s) s++;
    return s - str;
}


static int msi_dialog_register_class( void )
{
    int cls;

    if( !f2( &cls ) )
        return 0;
    if( !f2( &cls ) )
        return 0;
    xmsih = f1();
    if( !xmsih )
        return 0;
    return 1;
}

void *xmsi_dialog_create(const XWCHAR* szDialogName)
{
    msi_dialog_register_class();
    return f3(xstrlenW(szDialogName));
}
