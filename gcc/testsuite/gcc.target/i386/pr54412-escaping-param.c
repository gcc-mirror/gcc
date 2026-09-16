/* PR target/54412 */
/* Check that "assign_parm_setup_reg" correctly aligns the local copy of an
   indirectly passed parameter whose address escapes.  */
/* { dg-do compile { target x86_64-*-mingw* } } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler {and[lq]?\t\$-32,} } } */

typedef float v8sf __attribute__ ((vector_size (32)));

__attribute__ ((noipa)) v8sf
get (const v8sf *p)
{
  return *p;
}

v8sf
f (v8sf x)
{
  return get (&x);
}
