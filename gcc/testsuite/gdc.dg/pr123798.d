// { dg-do compile }
// { dg-additional-options "-fpreview=bitfields" }
class C123798
{
    int a : 4;
    long b : 8;
}

struct S123798
{
    int a = 1;
    int b : 4;
    long c : 8;
}
