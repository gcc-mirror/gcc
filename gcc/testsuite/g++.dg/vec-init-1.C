/* On S/390 this ends up calling the vec_init RTL expander with a
   parallel of two symbol_refs.  */

/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O3 -fPIC" } */


struct test
{
    struct base
    {
       int key;
    };
    struct derived : public base
    {
       int key;
    };

    derived core;
    derived &dRef;
    base &bRef;

    test() : dRef (core), bRef (core) {}
};

test test;
