/* PR preprocessor/28709 */
/* { dg-do preprocess } */

#define foo - ## >>
foo		/* { dg-error "pasting \"-\" and \">>\"" } */
#define bar = ## ==
bar		/* { dg-error "pasting \"=\" and \"==\"" } */
