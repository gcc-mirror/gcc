// { dg-do compile }
// { dg-additional-options "-fexceptions -fnon-call-exceptions" }
// PR tree-optimization/116601

struct RefitOption {
  char subtype;
  int string;
} n;
void h(RefitOption);
void k(RefitOption *__val)
{
  try {
    *__val = RefitOption{};
    RefitOption __trans_tmp_2 = *__val;
    h(__trans_tmp_2);
  }
  catch(...){}
}
