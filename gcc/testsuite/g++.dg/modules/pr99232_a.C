// PR c++/99232
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi pr99232 }

export module pr99232;

export const double lambda{ 1.3 };
export constexpr int a = 42;

export const double* get_lambda_addr() {
  return &lambda;
}
