/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target lp64 } */

#define TYPE void*
#define TYPE2 void*

struct path {
    TYPE2 mnt;
    TYPE dentry;
};

struct nameidata {
    struct path path;
    struct path root;
};

__attribute__ ((noinline))
TYPE foo(struct nameidata *nd)
{
  TYPE d;
  TYPE2 d2;

  nd->path = nd->root;
  d = nd->path.dentry;
  d2 = nd->path.mnt;
  return d;
}

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mstxv\M} } } */
/* { dg-final { scan-assembler-times {\mld\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstd\M} 2 } } */
