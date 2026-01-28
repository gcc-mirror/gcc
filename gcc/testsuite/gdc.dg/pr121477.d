// { dg-do compile }
// { dg-additional-options "-fpreview=bitfields" }
struct S121477
{
    int x : 4;
}

void f121477(S121477 s)
{
    asm {"%0" :: "m" (s.x); } // { dg-error "cannot take address of bit-field" }
    asm {"%0" : "=m" (s.x); } // { dg-error "cannot take address of bit-field" }
}
