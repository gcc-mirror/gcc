/* { dg-do compile } */
/* { dg-options "--completion=--param=asan-" } */

/* { dg-begin-multiline-output "" }
--param=asan-globals=
--param=asan-instrument-allocas=
--param=asan-instrument-reads=
--param=asan-instrument-writes=
--param=asan-instrumentation-with-call-threshold=
--param=asan-memintrin=
--param=asan-stack=
--param=asan-use-after-return=
   { dg-end-multiline-output "" } */
