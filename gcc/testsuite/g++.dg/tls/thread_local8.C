// PR c++/55800
// { dg-options "-std=c++11" }
// { dg-require-alias "" }
// { dg-require-effective-target tls }
// { dg-final { scan-assembler "_ZTH12foo_instance" { target tls_native } } }

struct foo
{
  foo();
};

thread_local foo foo_instance;
