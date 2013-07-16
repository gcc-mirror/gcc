/* { dg-do compile } */
/* { dg-options "-fno-delete-null-pointer-checks" } */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

extern struct module __this_module;
static inline void
trace_module_get  (struct module *mod, uintptr_t ip) { }
struct module;
static inline __attribute__((no_instrument_function))
int try_module_get(struct module *module)
{
  int ret = 1;
  if (module)
    {
      if (module_is_live(module))
	{
	  __label__ __here;
	  asm("");
	  __here:
	  trace_module_get(module, (uintptr_t)&&__here);
	}
      else
	ret = 0;
    }
  return ret;
}
struct net_device;
struct net_device_ops {
    int (*ndo_open)(struct net_device *dev);
};
int t3e3_open(struct net_device *dev)
{
  int ret = hdlc_open(dev);
  if (ret)
    return ret;
  try_module_get((&__this_module));
  return 0;
}
const struct net_device_ops t3e3_ops = { .ndo_open = t3e3_open };
