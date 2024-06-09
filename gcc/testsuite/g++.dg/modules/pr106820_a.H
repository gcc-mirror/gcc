// PR c++/106820
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi {} }

static int __gthrw___pthread_key_create() __attribute__((__weakref__("foo")));
