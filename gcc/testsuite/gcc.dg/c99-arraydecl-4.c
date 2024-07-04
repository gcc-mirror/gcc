/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

void fo(char buf[static]);	/* { dg-error "'static' may not be used without an array size" } */
void fo(char buf[static]) { }	/* { dg-error "'static' may not be used without an array size" } */

void fu(char buf[static *]);	/* { dg-error "'static' may not be used with an unspecified variable length array size" } */
void fu(char buf[static *]) { }	/* { dg-error "'static' may not be used with an unspecified variable length array size" } */

void fe(int n, char buf[static n]);
void fe(int n, char buf[static *]) { }	/* { dg-error "'static' may not be used with an unspecified variable length array size" } */

void fa(int *n, char buf[static *n]);
void fa(int *n, char buf[static *n]) { }
