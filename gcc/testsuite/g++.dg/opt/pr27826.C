/* { dg-do compile } */
/* { dg-options "-O3" } */

struct Geometry
{
        int type:16;
};
struct Geometry get() { return Geometry(); };
int f()
{
        struct Geometry test;
        return get().type == test.type;
}

