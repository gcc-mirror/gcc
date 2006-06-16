/* { dg-do compile } */
/* { dg-options "-O" } */

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

