module b20885;

struct S
{
    alias P  = void*;
}

void main()
{
    alias P  = void*;
    alias PP = void**;
    PP[1] a  = null;
    if (const void** b = a[0]){} // OK
    if (const P*     b = a[0]){} // NG
    if (const S.P*   b = a[0]){} // NG
}
