/* { dg-do compile } */
/* { dg-options "-Wformat-truncation" } */

/* A run of blank lines, so that we would fail the assertion in input.c:1388:
   gcc_assert (line_width >= (start.column - 1 + literal_length));  */





void test (void)
{
  char tmp[128];
  /* Point to the run of blank lines, so that the components of the overlong
     string appear to be present within the run of blank lines.  */
# 6 "../../../../src/gcc/testsuite/gcc.dg/format/pr78569.c"
  __builtin_snprintf (tmp, sizeof(tmp),
		      "The Base Band sends this value as a response to a "
		      "request for IMSI detach sent over the control "
		      "channel uplink (see section 7.6.1).");

  /* { dg-warning "output truncated" "" { target *-*-* } 7 } */
  /* { dg-message "output" "" { target *-*-* } 6 } */
}
