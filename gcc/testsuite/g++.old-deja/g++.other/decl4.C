// Build don't link:
// crash test - XFAIL *-*-*

// Reported by Harri Porten <porten@tu-harburg.de>
// Simplified for testsuite by Alexandre Oliva

struct foo { operator long double(); };
int bar(int __opr); // gets bogus error - XFAIL *-*-*
