namespace a {
    namespace b {
        void foo();
    }
}

void
a::b:foo() // { dg-error "" }
{
}
