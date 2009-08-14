/* PR c++/14962 */
/* Originator: <phil@fsel.com> */

/* { dg-final { scan-assembler "new_name" } } */
/* { dg-final { scan-assembler-not "old_name" } } */

#ifndef __PRAGMA_REDEFINE_EXTNAME
#error 
#endif

extern "C" {

struct old_name { int i; };

#pragma redefine_extname old_name new_name

int old_name(void);

}

int foo(void)
{
  return old_name();
}
