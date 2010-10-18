/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

typedef unsigned char uint8_t;
typedef uint8_t foo[24];

void thingy(foo a)
{
}

int main()
{
    foo bar;

    @try {
    } 
    @finally {
    }

    thingy(bar);

    return 0;
}

