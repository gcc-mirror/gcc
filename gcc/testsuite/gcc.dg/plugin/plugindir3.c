/* { dg-do preprocess } */
/* { dg-options "-fplugin=foo" } */

/* { dg-prune-output ".*inaccessible plugin file.*foo\.so expanded from short plugin name.*" } */
