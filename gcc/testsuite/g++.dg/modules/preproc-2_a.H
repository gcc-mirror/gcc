# 0 "preproc-2_a.H"
# 0 "<built-in>"
#define __STDC__ 1
# 0 "<command-line>"
#define NAME bob
# 1 "preproc-2_a.H"
# 8 "preproc-2_a.H"
void NAME ();

// { dg-additional-options "-fmodules-ts -fpreprocessed -fdirectives-only" }
// { dg-module-cmi {,/preproc-2_a.H} }
