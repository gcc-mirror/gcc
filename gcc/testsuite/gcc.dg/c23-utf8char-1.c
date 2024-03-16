/* Test C23 UTF-8 characters.  Test valid usages.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

unsigned char a = u8'a';
_Static_assert (u8'a' == 97);

unsigned char b = u8'\0';
_Static_assert (u8'\0' == 0);

unsigned char c = u8'\xff';
_Static_assert (u8'\xff' == 255);

unsigned char d = u8'\377';
_Static_assert (u8'\377' == 255);

_Static_assert (sizeof (u8'a') == 1);
_Static_assert (sizeof (u8'\0') == 1);
_Static_assert (sizeof (u8'\xff') == 1);
_Static_assert (sizeof (u8'\377') == 1);

_Static_assert (_Generic (u8'a', unsigned char: 1, default: 2) == 1);
_Static_assert (_Generic (u8'\0', unsigned char: 1, default: 2) == 1);
_Static_assert (_Generic (u8'\xff', unsigned char: 1, default: 2) == 1);
_Static_assert (_Generic (u8'\377', unsigned char: 1, default: 2) == 1);

#if u8'\0' - 1 < 0
#error "UTF-8 constants not unsigned in preprocessor"
#endif
