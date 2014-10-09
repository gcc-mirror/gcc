/* PR63300 make sure we don't duplicate type qualifiers unneeded.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

/* This should give us:
   - One const type pointing to a char
   - One volatile type pointing to a char
   - Either one const type pointing to the volatile type pointing to a char
     or one volatile type pointing to the const type pointing to a char.
     But not both.  */

char a;
const char b;
volatile const char c;
volatile char d;
const volatile char e;

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_(?:const|volatile)_type" 3 } } */
