/* { dg-do compile } */
/* { dg-options "-mtiny-printf" } */
/* { dg-error "GCC must be configured with --enable-newlib-nano-formatted-io to use -mtiny-printf" "" { target { ! newlib_nano_io } } 0 } */
