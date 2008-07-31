/* PR 24931 */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

struct p {
        short x, y;
};

struct s {
        int i;
        struct p p;
};

struct s f()
{
        struct s s;
        s.p = (struct p){};
        s.i = (s.p.x || s.p.y);
        return s;
}
