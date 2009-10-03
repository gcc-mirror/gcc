// { dg-lto-do assemble }
// Test case from Eugene A. Strizhov.

extern int i;
struct S { S (); };

S::S ()
{
    enum { fifty = 0x50 };
    if (i > fifty);
}
