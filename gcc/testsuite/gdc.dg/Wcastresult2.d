// { dg-do compile }
// { dg-options "-Wcast-result -Wno-deprecated" }

void test()
{
    auto imvalue = 1.23i;
    auto revalue = 1.23;

    auto imtore = cast(double)imvalue; // { dg-warning "cast from 'idouble' to 'double' will produce zero result" }
    auto retoim = cast(idouble)revalue; // { dg-warning "cast from 'double' to 'idouble' will produce zero result" }
    return;
}
