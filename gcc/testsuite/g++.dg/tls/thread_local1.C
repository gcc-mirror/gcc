// { dg-options "-std=c++11" }
// { dg-require-effective-target tls }

// The variable should have a guard.
// { dg-final { scan-assembler "_ZGVZ1fvE1a" } }
// But since it's thread local we don't need to guard against
// simultaneous execution.
// { dg-final { scan-assembler-not "cxa_guard" } }
// The guard should be TLS, not local common.
// { dg-final { scan-assembler-not "\.comm" } }

struct A
{
  A();
};

A &f()
{
  thread_local A a;
  return a;
}
