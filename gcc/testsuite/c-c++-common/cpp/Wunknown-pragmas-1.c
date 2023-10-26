/* PR c++/89038 */
/* { dg-additional-options "-Wunknown-pragmas" } */

#pragma oops /* { dg-warning "-:-Wunknown-pragmas" } */
#pragma GGC diagnostic push /* { dg-warning "-:-Wunknown-pragmas" } */
#pragma GCC diagnostics push /* { dg-warning "-:-Wunknown-pragmas" } */

/* Test we can disable the warnings.  */
#pragma GCC diagnostic ignored "-Wunknown-pragmas"

#pragma oops /* { dg-bogus "-:-Wunknown-pragmas" } */
#pragma GGC diagnostic push /* { dg-bogus "-:-Wunknown-pragmas" } */
#pragma GCC diagnostics push /* { dg-bogus "-:-Wunknown-pragmas" } */
