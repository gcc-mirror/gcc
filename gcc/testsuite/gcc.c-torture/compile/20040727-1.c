/* Extracted from boehm-gc/os_dep.c on Darwin.  It caused an ICE when
   trying to merge alias information from two pointers that had
   different type memory tags.  */
typedef int thread_state_flavor_t;
typedef int exception_behavior_t;
typedef unsigned int exception_mask_t;
typedef unsigned int exception_handler_t;
typedef unsigned int mach_msg_type_number_t;
static struct {
   mach_msg_type_number_t count;
   exception_mask_t masks[16];
   exception_handler_t ports[16];
   thread_state_flavor_t flavors[16];
} GC_old_exc_ports;

typedef exception_handler_t *exception_handler_array_t;
typedef thread_state_flavor_t *exception_flavor_array_t;


int task_get_exception_ports
(
  mach_msg_type_number_t *masksCnt,
  exception_handler_array_t old_handlers,
  exception_flavor_array_t old_flavors
);

void GC_dirty_init()
{
   task_get_exception_ports(GC_old_exc_ports.masks,
                           GC_old_exc_ports.ports,
                           GC_old_exc_ports.flavors);
}
