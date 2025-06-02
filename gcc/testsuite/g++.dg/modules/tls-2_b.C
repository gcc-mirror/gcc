// PR c++/120363
// { dg-additional-options "-fmodules" }

module M;
thread_local int test::instance;
