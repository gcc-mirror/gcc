/* Adapted from include/linux/math.h  */
#define __round_mask(x, y) ((__typeof__(x))((y)-1))
#define round_up(x, y) ((((x)-1) | __round_mask(x, y))+1)

/* Reduced from Linux kernel's drivers/gpu/drm/i915/display/intel_bios.c  */
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long __kernel_size_t;
typedef __kernel_size_t size_t;

extern __attribute__((__alloc_size__(1))) __attribute__((__malloc__))
void* kzalloc(size_t size);

typedef struct
{
  u32 reg;
} i915_reg_t;
struct intel_uncore;
struct intel_uncore_funcs
{
  u32 (*mmio_readl)(struct intel_uncore* uncore, i915_reg_t r);
};
struct intel_uncore
{
  void* regs;
  struct intel_uncore_funcs funcs;
};
static inline __attribute__((__gnu_inline__)) __attribute__((__unused__))
__attribute__((no_instrument_function)) u32
intel_uncore_read(struct intel_uncore* uncore, i915_reg_t reg)
{
  return uncore->funcs.mmio_readl(uncore, reg);
}
struct drm_i915_private
{
  struct intel_uncore uncore;
};
struct vbt_header*
spi_oprom_get_vbt(struct drm_i915_private* i915)
{
  u16 vbt_size;
  u32* vbt;
  vbt_size =
    intel_uncore_read(&i915->uncore, ((const i915_reg_t){ .reg = (0x102040) }));
  vbt_size &= 0xffff;
  vbt = (u32*)kzalloc(round_up (vbt_size, 4)); /* { dg-bogus "allocated buffer size is not a multiple of the pointee's size" "PR analyzer/113654" } */
  if (!vbt)
    goto err_not_found;
  return (struct vbt_header*)vbt;
err_not_found:
  return ((struct vbt_header*)0);
}
