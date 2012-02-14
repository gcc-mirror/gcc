/* PR c/52181 */
/* { dg-do compile } */

extern const int v1[];
const int __attribute__((aligned(16))) v1[] = { 1 };
extern const int __attribute__((aligned(16))) v2[];
const int v2[] = { 1 };
extern const int __attribute__((aligned(16))) v3[];
const int __attribute__((aligned(16))) v3[] = { 1 };
const int __attribute__((aligned(16))) v4[] = { 1 };
int test[(__alignof__ (v4) != __alignof__ (v1)		/* { dg-bogus "is negative" } */
	 || __alignof__ (v4) != __alignof__ (v2)
	 || __alignof__ (v4) != __alignof__ (v3)) ? -1 : 0];
