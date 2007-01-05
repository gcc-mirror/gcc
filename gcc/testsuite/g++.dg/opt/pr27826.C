/* { dg-do compile } */
/* { dg-options "-O3" } */

struct Geometry
{
        int type:16;
};
struct Geometry get() {};
int f()
{
        struct Geometry test;
        return get().type == test.type;
}

