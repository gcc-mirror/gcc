/* This caused an out-of-range address on the MIPS port.  */
void foo (char *x) { __builtin_prefetch (x + 0x8000); }
