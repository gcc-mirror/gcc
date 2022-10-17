// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101672
// { dg-do compile }

module object;

interface I101672
{
    static int i101672;
}

class A101672 : I101672
{
    static int a101672;
}

class B101672 : A101672
{
    static int b101672;
}
