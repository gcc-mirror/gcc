/* PR preprocessor/58844 */
/* { dg-do compile } */
/* { dg-options "-ftrack-macro-expansion=2" } */

#define A x######x
int A = 1;
#define A x######x	/* { dg-message "-:previous definition" } */
#define A x##x		/* { dg-warning "-:redefined" } */
