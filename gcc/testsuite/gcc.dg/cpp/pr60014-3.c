/* { dg-do compile } */
/* { dg-options "-save-temps -fpermissive -Wint-conversion" } */
#include "pr60014-3.h"

/* The line continuation on the next line is what triggers the problem here,
   because it synchronizes the output line between the input source and the
   preprocessed output (whereas without the line continuation, the
   preprocessed output would be off by one line from having output a #pragma
   on a line by itself). Therefore, the token streamer doesn't have a reason
   to generate a line marker purely based on the line number. That gives it
   the chance to consider whether instead it needs to generate a line marker
   based on a change of the "in-system-header" state, allowing us to test that
   it comes to the right conclusion, which it did not, prior to this commit to
   resolve PR60014.  */
P(GCC diagnostic) \
const char *should_warn = 1; /* { dg-warning {-Wint-conversion} } */
