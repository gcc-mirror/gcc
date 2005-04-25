/* { dg-do compile } */
/* { dg-options "-O2" } */

template<int N> void foo()
{
    double d = (N ? 0.0 : 0) + 1;
}

