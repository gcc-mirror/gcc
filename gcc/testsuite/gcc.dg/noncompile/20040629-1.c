/* PR 16216 */

void func()
{
        const char *pek; int i;
        pek=__builtin_va_arg(ap,const char*);	/* { dg-error "" } */
}
