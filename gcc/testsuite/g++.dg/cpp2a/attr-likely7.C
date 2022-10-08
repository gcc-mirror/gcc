// { dg-do compile { target c++20 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "hot label" 5 "gimple" } }
// { dg-final { scan-tree-dump-times "cold label" 3 "gimple" } }

bool b;

template <class T> int f()
{
  if (b)
    [[likely, whatever::unlikely ("abcd")]] return 0;		// { dg-bogus "ignoring attribute 'unlikely' after earlier 'likely'" }
  else								// { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
    [[unlikely, whatever2::hot]] flabel: return 1;		// { dg-warning "'whatever2::hot' scoped attribute directive ignored" }
  switch (b)
    {
      [[likely, whatever3::cold (1, 2, 3)]] case true: break;	// { dg-warning "'whatever3::cold' scoped attribute directive ignored" }
    };
  return 1;
}

int main()
{
  if (b)
    [[whatever4::unlikely (1), likely]] return 0;		// { dg-bogus "ignoring attribute 'likely' after earlier 'unlikely'" }
  else if (b)							// { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
    [[whatever5::hot, unlikely]] elabel:			// { dg-warning "'whatever5::hot' scoped attribute directive ignored" }
      return 1;
  else
    [[whatever6::cold, likely]] b = false;			// { dg-bogus "ignoring attribute 'likely' after earlier 'cold'" }
								// { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  f<int>();

  switch (b)
    {
      [[whatever7::unlikely (1), likely]] case true: break;	// { dg-warning "'whatever7::unlikely' scoped attribute directive ignored" }
      [[whatever8::unlikely, unlikely]] case false: break;	// { dg-bogus "attribute 'unlikely' specified multiple times" }
    };								// { dg-warning "'whatever8::unlikely' scoped attribute directive ignored" "" { target *-*-* } .-1 }
}
