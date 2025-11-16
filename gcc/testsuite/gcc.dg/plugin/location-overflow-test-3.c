/* { dg-do compile } */
/* { dg-options "-fplugin-arg-location_overflow_plugin-value=1024 -fdump-internal-locations" } */

/* The plugin arranges for location_t values to exceed 32 bits; verify the
   internal dump routines don't crash. The exact output depends on the system
   and on absolute path names, and this output is only meant for internal
   purposes, so don't demand an exact form of the output.  */

/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */
