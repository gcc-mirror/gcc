// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96254
// { dg-do compile }
mixin template test()
{
    int next;
}

void foo(alias l)()
{
    l.next = 0; // { dg-error "cannot get frame pointer to 'D main'" }
}

void bar(alias l, alias t)()
{
    l.next = 0; // { dg-error "cannot get frame pointer to 'D main'" }
}

void main()
{
    mixin test l1;
    mixin test l2;
    foo!(l1);
    bar!(l1,l2);
}
