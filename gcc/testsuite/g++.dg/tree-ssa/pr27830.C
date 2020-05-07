/* { dg-do compile } */
/* { dg-options "-O" } */
/* Ignore ABI warnings for C++17 and later.  */
/* { dg-additional-options "-Wno-psabi -w" { target c++17 } } */

struct gc{};
struct transform:public gc
{
    double x, y, z, t;
    transform (void){}
};
inline transform f (void)
{
    return transform ();
};
void transformed (void)
{
    new transform (f());
}

