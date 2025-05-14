// { dg-do "compile" }
// { dg-additional-options "-fonly=imports/fonly" }
// { dg-additional-sources "imports/fonly.d" }
// { dg-final { scan-assembler-not "_D1a10fonly_testFZv" } }
// { dg-final { scan-assembler "_D1b10fonly_testFZv" } }
module a;

void fonly_test() { }
