/* Regression test for ICE with -fanalyzer-verbose-state-changes.  */

/* { dg-additional-options " -fanalyzer-verbose-state-changes" } */

__builtin_va_list FOO_showfatal_ap;
void FOO_showfatal(char fmta, ...) {
  __builtin_va_start(FOO_showfatal_ap, fmta); /* { dg-message "'va_start' called here" } */
} /* { dg-warning "missing call to 'va_end'" } */
