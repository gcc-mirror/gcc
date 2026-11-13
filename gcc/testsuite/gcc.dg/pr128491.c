/* { dg-do assemble } */
/* { dg-options "-fgnu-tm" } */
inline void __attribute__ ((strub ("internal"), always_inline)) inl (void) { }
void bat (void) { inl (); } // { dg-error "" }
