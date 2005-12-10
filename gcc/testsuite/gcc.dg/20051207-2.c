/* GCC doesn't generate any .eh_frame data for this TU itself, so it
   shouldn't warn about "a" conflicting with the built-in idea of
   .eh_frame, even if it thinks that .eh_frame should be read-write.  */
/* { dg-require-named-sections "" } */
const int a __attribute__((section (".eh_frame"))) = 1;
