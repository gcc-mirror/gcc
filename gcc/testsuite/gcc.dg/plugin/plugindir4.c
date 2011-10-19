/* { dg-do preprocess } */
/* { dg-options "-iplugindir=my-plugindir -fplugin=foo" } */

/* { dg-prune-output ".*inaccessible plugin file.*my-plugindir/foo\.so expanded from short plugin name.*" } */
