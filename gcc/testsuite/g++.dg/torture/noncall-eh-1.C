// { dg-do compile }
// For slim LTO there's no optimized dump
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }
// { dg-additional-options "-fnon-call-exceptions -fexceptions -fdump-tree-optimized-eh" }

// PR tree-optimization/120599
// Copying prop for aggregates should not touch `a = *__val` since that statement
//  can throw (internally) so we need to be able to keep the landing pad.

struct RefitOption {
  char subtype;
  int string;
} n;
void h(RefitOption) __attribute__((nothrow));
void k(RefitOption *__val, RefitOption a)
{
  try {
    a = *__val;
    RefitOption __trans_tmp_2 = a;
    h(__trans_tmp_2);
  }
  catch(...){}
}

// Make sure There is a landing pad for the non-call exception from the aggregate load.
// { dg-final { scan-tree-dump "LP " "optimized" } }
