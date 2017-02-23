/* PR c/79428 */
int i;
#pragma GCC pch_preprocess /* { dg-error "'#pragma GCC pch_preprocess' must be first" } */
