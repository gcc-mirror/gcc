/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-std=iso9899:199409" } */

/* Just simple check that digraphs are on under c94, for both
   preprocessor and compiler.  digraphs.c is the general test.  */

%:define glue
#ifndef glue
#error glue not defined!
#endif

int main (int argc, char *argv<::>)
<%
  return 0;
%>
