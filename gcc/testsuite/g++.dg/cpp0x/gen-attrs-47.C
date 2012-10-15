// { dg-do compile { target c++11 } }
// Example taken from dcl.attr.grammar:

int p[10];
void f()
{
    int x = 42, y[5];
    /* Here, the '[[' should have introduced an attribute, on a
       lambda invocation an array subscripting expression.  */
    int(p[[x] { return x; }()]); // { dg-error "expected|consecutive" }
    /* Likewise, the '[[gnu::' is invalid here.  */
    y[[] { return 2; }()] = 2; // { dg-error "expected|consecutive" }
}
