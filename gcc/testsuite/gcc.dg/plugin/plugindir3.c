/* { dg-do preprocess } */
/* { dg-options "-fplugin=foo" } */

/* { dg-prune-output ".*inaccessible plugin file.*foo\.(so|dylib) expanded from short plugin name.*" } */
