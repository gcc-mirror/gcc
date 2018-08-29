/* { dg-do compile } */
/* { dg-options "--completion=--param=asan-" } */

/* { dg-begin-multiline-output "" }
--param=asan-stack
--param=asan-instrument-allocas
--param=asan-globals
--param=asan-instrument-writes
--param=asan-instrument-reads
--param=asan-memintrin
--param=asan-use-after-return
--param=asan-instrumentation-with-call-threshold
   { dg-end-multiline-output "" } */
