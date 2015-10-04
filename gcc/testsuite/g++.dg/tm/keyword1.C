// Test that these aren't keywords without -fgnu-tm.

int main()
{
  synchronized { }		// { dg-error "not declared" }
  atomic_noexcept { }		// { dg-error "not declared" }
  atomic_cancel { }		// { dg-error "not declared" }
  atomic_commit { }		// { dg-error "not declared" }
}
