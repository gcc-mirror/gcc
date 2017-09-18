/* make sure we don't duplicate type qualifiers unneeded.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -gdwarf-5 -dA" } */

/* This should give us:
   - One const type pointing to a char
   - One volatile type pointing to a char
   - One atomic type pointing to a char
   - Either one const type pointing to the volatile type pointing to a char
     or one volatile type pointing to the const type pointing to a char.
     But not both.
   - Either one volatile type pointing to an atomic type pointing to a char
     or one atomic type pointing to a volatile type pointing to a char.
     But not both.
   - One restrict type pointing to a char pointer.
   - One atomic type pointing to a char pointer.
   - Either one restrict type pointing to an atomic type pointing to a char
     pointer or one atomic type pointing to a restrict type pointing to a
     char pointer.
     But not both.  */


char a;
const char b;
volatile const char c;
volatile char d;
const volatile char e;
_Atomic char f;
_Atomic volatile char g;
char * _Atomic restrict h;
char * _Atomic i;
char * restrict j;

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_(?:const|volatile|atomic|restrict)_type" 8 } } */
