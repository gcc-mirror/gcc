/* { dg-do preprocess } */
/* { dg-options "-iplugindir=my-plugindir -fplugin=foo" } */

/* { dg-prune-output ".*inacessible plugin file.*my-plugindir/foo\.so expanded from short plugin name.*" } */
