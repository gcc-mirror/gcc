// { dg-do "compile" }
// { dg-additional-options "-fonly=driver_fonly2.d" }
// { dg-additional-sources "imports/fonly.d" }
// { dg-final { scan-assembler "_D1a10fonly_testFZv" } }
// { dg-final { scan-assembler-not "_D1b10fonly_testFZv" } }
module a;

void fonly_test() { }
