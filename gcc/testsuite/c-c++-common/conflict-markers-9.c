/* It's valid to have
<<<<<<<
   inside both
   comments (as above), and within string literals.  */
const char *s = "\
<<<<<<<";

/* The above shouldn't be reported as errors.  */
