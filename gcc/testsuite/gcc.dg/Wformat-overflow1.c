/*
    { dg-do compile }
    { dg-options "-Wformat-overflow -std=c2x" }
*/

extern int sprintf (char* restrict, const char* restrict, ...);

void test_warn () {

    int n = __INT_MAX__;
    char dst [5] = {0};
    sprintf (dst, "%b", n);  /* { dg-warning "-Wformat-overflow" } */

    sprintf (dst, "%#b", n); /* { dg-warning "-Wformat-overflow" } */

}

void test_no_warn () {

    char dst [5] = {0};
    int n = 8;
    sprintf (dst, "%b", n);

    char another_dst [34] = {0};
    n = __INT_MAX__;
    sprintf (another_dst, "%#b", n);

}
