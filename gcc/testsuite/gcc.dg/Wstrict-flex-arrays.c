/* Test -Wstrict-flex-arrays.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-flex-arrays -fstrict-flex-arrays=1" } */

struct trailing_array_1 {
    int a;
    int b;
    int c[4]; 
};

struct trailing_array_2 {
    int a;
    int b;
    int c[1]; 
};

struct trailing_array_3 {
    int a;
    int b;
    int c[0];
};
struct trailing_array_4 {
    int a;
    int b;
    int c[];
};

void __attribute__((__noinline__)) stuff(
    struct trailing_array_1 *normal,
    struct trailing_array_2 *trailing_1,
    struct trailing_array_3 *trailing_0,
    struct trailing_array_4 *trailing_flex)
{
    normal->c[5] = 5; 	/*{ dg-warning "should not be used as a flexible array member" } */
    trailing_1->c[2] = 2; /* { dg-bogus "should not be used as a flexible array member" } */
    trailing_0->c[1] = 1; /* { dg-bogus "should not be used as a flexible array member" } */
    trailing_flex->c[10] = 10; /* { dg-bogus "should not be used as a flexible array member" } */

}
