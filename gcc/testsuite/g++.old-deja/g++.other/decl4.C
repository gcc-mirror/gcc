// Build don't link:

// Reported by Harri Porten <porten@tu-harburg.de>
// Simplified for testsuite by Alexandre Oliva

struct foo { operator long double(); };
int bar(int __opr);
int bar(int __ope);
