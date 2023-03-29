/* Reduced from linux-5.10.162's drivers-base-bus.c  */
/* { dg-additional-options "-fno-delete-null-pointer-checks -O2" } */

#define NULL ((void*)0)

typedef unsigned int __kernel_size_t;
typedef int __kernel_ssize_t;
typedef __kernel_size_t size_t;
typedef __kernel_ssize_t ssize_t;

struct list_head
{
  struct list_head *next, *prev;
};

struct kobject
{
  /* [...snip...] */
};

struct attribute
{
  /* [...snip...] */
};

static inline
void
sysfs_remove_file_ns(struct kobject* kobj,
                     const struct attribute* attr,
                     const void* ns)
{
}

static inline
void
sysfs_remove_file(struct kobject* kobj, const struct attribute* attr)
{
  sysfs_remove_file_ns(kobj, attr, NULL);
}

extern struct kobject*
kobject_get(struct kobject* kobj);

extern void
kobject_put(struct kobject* kobj);

struct kset
{
  struct list_head list;
  /* [...snip...] */
  struct kobject kobj;
  /* [...snip...] */
} __attribute__((__designated_init__));

static inline
struct kset*
to_kset(struct kobject* kobj)
{
  return kobj ? ({
    void* __mptr = (void*)(kobj);
    ((struct kset*)(__mptr - __builtin_offsetof(struct kset, kobj)));
  }) : NULL;
}

static inline
struct kset*
kset_get(struct kset* k)
{
  return k ? to_kset(kobject_get(&k->kobj)) : NULL;
}

static inline
void
kset_put(struct kset* k)
{
  kobject_put(&k->kobj);
}

struct bus_type
{
  /* [...snip...] */
  struct device* dev_root;
  /* [...snip...] */
  struct subsys_private* p;
  /* [...snip...] */
};

struct bus_attribute
{
  struct attribute attr;
  /* [...snip...] */
};

extern void
device_unregister(struct device* dev);

struct subsys_private
{
  struct kset subsys;
  /* [...snip...] */
};

static struct bus_type*
bus_get(struct bus_type* bus)
{
  if (bus) { /* { dg-bogus "check of 'bus' for NULL after already dereferencing it" } */
    kset_get(&bus->p->subsys);
    return bus;
  }
  return NULL;
}

static void
bus_put(struct bus_type* bus)
{
  if (bus)
    kset_put(&bus->p->subsys);
}

void
bus_remove_file(struct bus_type* bus, struct bus_attribute* attr)
{
  if (bus_get(bus)) {
    sysfs_remove_file(&bus->p->subsys.kobj, &attr->attr);
    bus_put(bus);
  }
}

extern ssize_t
drivers_autoprobe_show(struct bus_type* bus, char* buf);

extern ssize_t
drivers_autoprobe_store(struct bus_type* bus, const char* buf, size_t count);

extern struct bus_attribute bus_attr_drivers_autoprobe;

static void
remove_probe_files(struct bus_type* bus)
{
  bus_remove_file(bus, &bus_attr_drivers_autoprobe);
  /* [...snip...] */
}

void
bus_unregister(struct bus_type* bus)
{
  /* [...snip...] */
  if (bus->dev_root) /* { dg-bogus "pointer 'bus' is dereferenced here" } */
    device_unregister(bus->dev_root);
  /* [...snip...] */
  remove_probe_files(bus);
  /* [...snip...] */
}
