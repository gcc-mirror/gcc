/* { dg-do preprocess } */

/* 6.9.3.11: ...If there  are sequences of preprocessing tokens within
   the list of arguments  that  would  otherwise  act  as  preprocessing
   directives, the behavior is undefined.

   I choose to make this a hard error.  It definitely should not cause
   a core dump.  */

#define foo(bar) bar

foo( blah	/* { dg-error "unterminated" "" } */
#undef foo	/* { dg-error "may not be used inside" "foo(#undef foo)" } */
     blah )
