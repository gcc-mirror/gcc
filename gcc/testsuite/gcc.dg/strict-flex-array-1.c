/* testing the correct usage of attribute strict_flex_array.  */   
/* { dg-do compile } */
/* { dg-options "-O2" } */


int x __attribute__ ((strict_flex_array (1))); /* { dg-error "'strict_flex_array' attribute may not be specified for 'x'" } */

int [[gnu::strict_flex_array(1)]] x; /* { dg-warning "'strict_flex_array' attribute does not apply to types" } */

struct trailing {
    int a;
    int c __attribute ((strict_flex_array)); /* { dg-error "wrong number of arguments specified for 'strict_flex_array' attribute" } */
};

struct trailing_1 {
    int a;
    int b;
    int c __attribute ((strict_flex_array (2))); /* { dg-error "'strict_flex_array' attribute may not be specified for a non-array field" } */
};

extern int d;

struct trailing_array_2 {
    int a;
    int b;
    int c[1] __attribute ((strict_flex_array (d))); /* { dg-error "'strict_flex_array' attribute argument not an integer" } */
};

struct trailing_array_3 {
    int a;
    int b;
    int c[0] __attribute ((strict_flex_array (5))); /* { dg-error "'strict_flex_array' attribute argument '5' is not an integer constant between 0 and 3" } */
};
