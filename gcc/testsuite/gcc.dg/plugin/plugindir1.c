/* { dg-do compile } */
/* { dg-options "-c -fplugin=foo" } */

/* { dg-prune-output ".*inaccessible plugin file.*foo\.(so|dylib) expanded from short plugin name.*" } */
