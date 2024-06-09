#define SIZE 16
char buf[SIZE];

__attribute__ ((tainted_args))
char test_sanitized_by_modulus (int val)
{
  return buf[val % SIZE]; /* { dg-bogus "use of attacker-controlled value" } */
}
