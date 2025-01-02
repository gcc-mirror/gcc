#include <stdio.h>
#include <stdlib.h>

struct bitfield {
	unsigned int field1 : 1;
	unsigned int field2 : 1;
	unsigned int field3 : 1;
};

__attribute__((noinline)) static void
set_field1_and_field2(struct bitfield *b) {
	b->field1 = 1;
	b->field2 = 1;
}

__attribute__((noinline)) static struct bitfield *
new_bitfield(void) {
	struct bitfield *b = (struct bitfield *)malloc(sizeof(*b));
	b->field3 = 1;
	set_field1_and_field2(b);
	return b;
}

int main(void) {
	struct bitfield *b = new_bitfield();
	if (b->field3 != 1)
		__builtin_abort ();
	return 0;
}
