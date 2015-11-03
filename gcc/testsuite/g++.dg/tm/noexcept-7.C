// FIXME the TS says atomic_noexcept calls abort, not terminate.
// { dg-options "-fgnu-tm" }

void f()
{
  atomic_noexcept { throw; }	// { dg-warning "terminate" }
}
