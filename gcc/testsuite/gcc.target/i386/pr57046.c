/* { dg-do run } */
/* { dg-options "-O2" } */

struct emac {
        unsigned reg[23];
};

struct mop {
        unsigned long long addr;
        unsigned int size;
};

unsigned int __attribute__((__noinline__))
level(const struct emac *obj)
{
	return 0;
}

void __attribute__((__noinline__))
info(struct emac *dev, unsigned long long addr)
{
	asm("" : : : "memory");
}

unsigned long long __attribute__((__noinline__))
get_value(const struct mop *mop)
{
        return 0x1234567890abcdefull;
}

int __attribute__((__noinline__))
emac_operation(struct emac *obj, struct mop *mop)
{
        unsigned long long addr = mop->addr;
        int index = addr >> 2;
	unsigned int value, old_value;

        if (mop->size != 4)
                return 0;

        if (index >= 23) {
                if (level(obj) >= 1)
                        info(obj, addr);
                return 0;
        }

        value = get_value(mop);
        old_value = obj->reg[index];

        info(obj, 0);

        switch (index) {
        case 0:
                obj->reg[0] = old_value;
                break;
        case 7:
        case 8:
                obj->reg[index] = value;
                break;
        }

        return 0;
}

int main(void)
{
	struct emac e = { { 0 } };
	struct mop mop = { 32, 4 };

	e.reg[8] = 0xdeadbeef;
	emac_operation(&e, &mop);

	if (e.reg[8] != 0x90abcdef)
	   __builtin_abort();

	   return 0;
}
