// { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } }
import gcc.builtins : va_list = __builtin_va_list;

void argpass(va_list *ap);

void pr110712a(va_list ap)
{
    argpass(&ap); // { dg-error "cannot convert parameter" }
}

void pr110712b(va_list ap)
{
    va_list ap2 = ap; // { dg-error "cannot convert parameter" }
}

struct pr110712c
{
    this(va_list ap)
    {
        this.ap = ap; // { dg-error "cannot convert parameter" }
    }
    va_list ap;
}
