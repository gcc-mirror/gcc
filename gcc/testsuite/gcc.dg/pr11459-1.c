/* -ansi -std=c99 should mean -std=c99, but the specs reordered the
    options.  Bug 11459.  */
/* { dg-do compile } */
/* { dg-options "-ansi -std=c99 -pedantic" } */

long long i;
