
void main()
{
    version(D_InlineAsm_X86) {}
    else version(D_InlineAsm_X64) {}
    else static assert(0);

    asm {
        fldz ST(0), ST(1), ST(2), ST(3);
        fld ST(0), ST(1), ST(2), ST(3);
    }
}
