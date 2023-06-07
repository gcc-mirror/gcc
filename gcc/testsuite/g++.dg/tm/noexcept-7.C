// FIXME the TS says atomic_noexcept calls abort, not terminate.
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "-fgnu-tm" }

void f()
{
  atomic_noexcept { throw; }	// { dg-warning "terminate" }
}
