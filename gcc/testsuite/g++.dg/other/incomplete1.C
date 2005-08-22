// PR c++/23089
// Origin: Flash Sheridan  <flash@pobox.com>
// ICE on incomplete type
// { dg-do compile }
// { dg-options "-O" }

void foo(struct A) {} // { dg-error "incomplete type|forward declaration" }
