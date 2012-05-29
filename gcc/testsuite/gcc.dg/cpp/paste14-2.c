/* PR preprocessor/28709 */
/* 
   { dg-options "-ftrack-macro-expansion=2" }
   { dg-do preprocess }
*/

#define foo - ## >> /* { dg-error "pasting \"-\" and \">>\"" } */
foo
#define bar = ## == /* { dg-error "pasting \"=\" and \"==\"" } */
bar

