/* PR preprocessor/28709 */
/* 
   { dg-options "-ftrack-macro-expansion=0" }
   { dg-do preprocess }
*/

#define foo - ## >>
foo		/* { dg-error "pasting \"-\" and \">>\"" } */
#define bar = ## ==
bar		/* { dg-error "pasting \"=\" and \"==\"" } */
