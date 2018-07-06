/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */

struct s { ~s() { s(); } };

int f()
{
  M:
    s o = s();
    f();
    f();

  L:
    goto *(f() ? &&L : &&M);

    return 0;
}
