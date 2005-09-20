/* PR rtl-optimization/22003 */
/* { dg-do compile } */
/* -freorder-blocks-and-partition is not supported on machines */
/* that do not support named sections.  */
/* { dg-require-named-sections "" } */
/* { dg-options "-O2 -fno-exceptions -freorder-blocks-and-partition" } */

struct c1
{
      virtual ~c1();
};
class c4;

struct c2
{
      virtual c4* func();
};

struct c3 : c1, c2
{
      c4* func();
};

c4* c3::func()
{
}

