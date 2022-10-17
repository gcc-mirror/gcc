// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101664
// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-sources "pr101664_1.cc" }

extern(C++) struct S101664
{
    int i;
    this(int);
}

void main()
{
    assert(S101664(1).i == 1);
}
