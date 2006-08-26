/* { dg-do compile } */
/* { dg-options "-O2 -Wswitch-enum" } */

typedef enum { a = 2 } T;

int main()
{
    T x = a;
    switch(x)
    {
    case a ... 3: /* { dg-warning "case value '3' not in enumerated" "3" } */
        break;
    }
    switch(x)
    {
    case 1 ... a: /* { dg-warning "case value '1' not in enumerated" "1" } */
        break;
    }
    return 0;
}

