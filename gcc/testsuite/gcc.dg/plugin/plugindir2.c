/* { dg-do compile } */
/* { dg-options "-save-temps -c -fplugin=foo" } */

/* { dg-prune-output ".*inaccessible plugin file.*foo\.so expanded from short plugin name.*" } */
