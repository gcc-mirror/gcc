/* { dg-do compile } */

extern int printf(const char *, ...);
struct list_head {
	struct list_head *next, *prev;
};
struct resource {
	unsigned long long start;
	unsigned long long end;
	unsigned long flags;
	struct resource *parent, *sibling, *child;
};
struct pci_dev {
	struct list_head bus_list;
	struct resource resource[12];
};
struct pci_bus {
	struct list_head devices;
	unsigned char secondary;
	unsigned char subordinate;
};
struct resource *pci_bus_resource_n(const struct pci_bus *bus, int n);
static struct resource *find_free_bus_resource(struct pci_bus *bus, unsigned long type) {
	int i;
	struct resource *r;
	unsigned long type_mask = 0x00000100 | 0x00000200 | 0x00002000;
	for (i = 0; (r = pci_bus_resource_n(bus, i)) || i < 4; i++) {
		if (r && (r->flags & type_mask) == type && !r->parent) return r;
	}
	return ((void *)0);
}
static unsigned long long calculate_memsize(unsigned long long size, unsigned long long min_size, unsigned long long size1, unsigned long long old_size, unsigned long long align) {
	if (old_size == 1 ) old_size = 0;
	if (size < old_size) size = old_size;
	return size;
}
void pbus_size_mem(struct pci_bus *bus, unsigned long mask, unsigned long type, unsigned long long min_size, unsigned long long add_size, void *realloc_head) {
	struct pci_dev *dev;
	unsigned long long min_align, align, size, size0, size1;
	int order;
	struct resource *b_res = find_free_bus_resource(bus, type);
	unsigned long long children_add_size = 0;
	if (!b_res) return;
	for (dev = ({
				const typeof( ((typeof(*dev) *)0)->bus_list ) *__mptr = ((&bus->devices)->next);
				(typeof(*dev) *)( (char *)__mptr - __builtin_offsetof(typeof(*dev),bus_list) );
				}
		   );
			&dev->bus_list != (&bus->devices);
			dev = ({
				const typeof( ((typeof(*dev) *)0)->bus_list ) *__mptr = (dev->bus_list.next);
				(typeof(*dev) *)( (char *)__mptr - __builtin_offsetof(typeof(*dev),bus_list) );
				}
			      )) {
		int i;
		for (i = 0; i < 12; i++) {
			struct resource *r = &dev->resource[i];
			unsigned long long r_size;
			if (r->parent || (r->flags & mask) != type) continue;
			r_size = r->end - r->start + 1;
			if (order > 11) {
				printf("%d: %pR %#llx\n", i, r, (unsigned long long) align);
			}
			size += r_size;
		}
	}
	if (children_add_size > add_size) add_size = children_add_size;
	size1 = (!realloc_head || (realloc_head && !add_size)) ? size0 : calculate_memsize(size, min_size+add_size, 0, b_res->end - b_res->start + 1, min_align);
	if (!size0 && !size1) {
		printf("%pR %02x-%02x\n", b_res, bus->secondary, bus->subordinate);
	}
}
