/* PR tree-optimizations/40351 */

struct IO_APIC_route_entry {
    unsigned int vector : 8;
    unsigned int delivery_mode : 1;
    unsigned int mask : 1;
    unsigned int __reserved_2 : 15;
    unsigned int __reserved_3 : 8;
} __attribute__ ((packed));
union entry_union {
    struct {
        unsigned int w1, w2;
    };
    struct IO_APIC_route_entry entry;
};
unsigned int io_apic_read(void);
struct IO_APIC_route_entry ioapic_read_entry(void)
{
  union entry_union eu;
  eu.w1 = io_apic_read();
  return eu.entry;
}
