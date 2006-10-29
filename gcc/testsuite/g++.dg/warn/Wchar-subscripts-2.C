/* PR c++/16307 */
// { dg-do compile }
// { dg-options "-Wchar-subscripts" }

char foo (const char *s)
{
    return s [s ['\x80']];          // { dg-warning "array subscript" }
}

int main ()
{
    foo ("\x80");
}
