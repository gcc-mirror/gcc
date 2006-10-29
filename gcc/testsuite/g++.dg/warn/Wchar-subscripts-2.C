/* PR c++/16307 */
// { dg-do compile }
// { dg-options "-Wchar-subscripts" }

extern volatile char bla;

char foo (const char *s)
{
    return s [bla];          // { dg-warning "array subscript" }
}

int main ()
{
    foo ("\x80");
}
