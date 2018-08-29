/* { dg-do compile { target c++11 } } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O0" } */

struct DArray
{
    __SIZE_TYPE__ length;
    int* ptr;
};

void foo35(DArray)
{
    static __thread int x[5];
    foo35({5, (int*)&x});
}
