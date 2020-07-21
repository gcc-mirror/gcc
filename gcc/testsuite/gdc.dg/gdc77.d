// https://bugzilla.gdcproject.org/show_bug.cgi?id=77
// { dg-do compile }

void fun(size_t n)(ubyte[n] val)
{
}

void test77(ubyte[3] buf)
{
    fun(buf[0..2]);
}
