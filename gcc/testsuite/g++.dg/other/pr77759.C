// PR target/77759
// This ICEd in the 64-bit SPARC back-end because of the nested empty struct.

// { dg-do compile }

struct empty {};

struct pair_empty
{
   struct empty a;
   struct empty b;
};

extern void foo (int slot0, int slot1, int slot2, int slot3, int slot4,
		 int slot5, struct pair_empty pair);

void bar (void)
{
  struct pair_empty pair;
  foo (0, 0, 0, 0, 0, 0, pair);
}
