/* PR63300 make sure we don't duplicate type qualifiers unneeded.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -gdwarf-4 -dA" } */

/* This should give us:
   - One restrict type pointing to a char pointer.
   - One volatile type pointing to the restrict type.
   - One const type pointing to the restrict type.
   - Either one const type pointing to the volatile type pointing to
     the restrict type or one volatile type pointing to the const type
     pointing to the restrict type.  But not both.  */

char * restrict a;
char * const restrict b;
char * const volatile restrict c;
char * volatile restrict d;

/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_restrict_type" 1 } } */
/* { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_(?:const|volatile)_type" 3 } } */
