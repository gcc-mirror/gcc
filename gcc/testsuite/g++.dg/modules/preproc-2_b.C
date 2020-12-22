# 0 "preproc-2_b.C"
# 0 "<built-in>"
#define __STDC__ 1
# 0 "<command-line>"
# 1 "preproc-2_b.C"
# 7 "preproc-2_b.C"
import "./preproc-2_a.H";

#define NAME bill

int NAME = 4;

int bob; // error with header

float NAME; // error with self

// { dg-additional-options "-fmodules-ts -fpreprocessed -fdirectives-only" }

// { dg-regexp {preproc-2_b.C:13:5: error: 'int bob' redeclared as different kind of entity\nIn module ./preproc-2_a.H, imported at preproc-2_b.C:7:\n<command-line>: note: previous declaration 'void bob\(\)'\npreproc-2_a.H:8:6: note: in expansion of macro 'NAME'} }

// { dg-regexp {preproc-2_b.C:9:14: error: conflicting declaration 'float bill'\npreproc-2_b.C:15:7: note: in expansion of macro 'NAME'\npreproc-2_b.C:9:14: note: previous declaration as 'int bill'\npreproc-2_b.C:11:5: note: in expansion of macro 'NAME'} }
