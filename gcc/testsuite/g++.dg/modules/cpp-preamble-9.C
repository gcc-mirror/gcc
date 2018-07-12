// { dg-do preprocess }
// { dg-additional-options -fmodule-preamble }





// { dg-final { scan-file cpp-preamble-9.i "# 9 \"\[^\n\]*cpp-preamble-9.C\"\nexport module hello.extra;\nimport hello;\n" } }
export module hello.extra;
import hello;
