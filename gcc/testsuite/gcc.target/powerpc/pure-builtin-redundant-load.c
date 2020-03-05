/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -fdump-tree-fre-all -mvsx" } */

/* Verify we remove a redundant load that occurs both before and after
we call a vector load builtin.
This testcase is introduced as we updated a number of our vector load
built-ins with the attribute of PURE instead of MEM, to indicate that
those builtins only read from memory, versus reading from and writing
to the same.
This means we can identify the redundant load instructions in an earlier
pass, and optimize them away.  */

#include <altivec.h>

vector signed short load_data;

vector signed short foo()
{
	vector signed short r11,r12,r13;
	r11 = load_data;
	r12 = vec_xl (0, &load_data[0]);
	r13 = load_data;
	return (r11 + r12 + r13);
}

vector signed short biz()
{
	vector signed short r21,r22,r23;
	r21 = load_data;
	r22 = vec_lvehx (0, &load_data[0]);
	r23 = load_data;
	return (r21 + r22 + r23);
}

vector signed short bar()
{
	vector signed short r31,r32,r33;
	r31 = load_data;
	r32 = vec_lvx (0, &load_data[0]);
	r33 = load_data;
	return (r31 + r32 + r33);
}

/* { dg-final { scan-tree-dump-times "Removing dead stmt r13_. = load_data;" 1 "fre1" } } */
/* { dg-final { scan-tree-dump-times "Removing dead stmt r23_. = load_data;" 1 "fre1" } } */
/* { dg-final { scan-tree-dump-times "Removing dead stmt r33_. = load_data;" 1 "fre1" } } */
