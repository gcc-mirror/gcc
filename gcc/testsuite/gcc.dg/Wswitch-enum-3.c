/* { dg-do compile } */
/* { dg-options "-O2 -Wswitch-enum" } */

typedef enum { a = 2 } T;

int main()
{
    switch((T)a) /* { dg-warning "enumeration value 'a' not handled" "a" } */
    {
    case 1: /* { dg-warning "case value '1' not in enumerated" "1" } */
        break;
    }
    return 0;
}

