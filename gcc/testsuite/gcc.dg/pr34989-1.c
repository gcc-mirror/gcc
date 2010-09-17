/* { dg-do link } */
/* { dg-require-effective-target-lto } */
/* { dg-options "-flto -r -nostdlib -O2" } */
/* { dg-additional-sources "pr34989-2.c" } */

extern struct globals *const ptr_to_globals;
struct globals { };
int syslogd_main(int argc, char **argv)
{
 (*(struct globals**)&ptr_to_globals) = 0;
}
