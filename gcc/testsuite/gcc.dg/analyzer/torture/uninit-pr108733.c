#define NULL ((void*)0)

typedef unsigned char __uint8_t;
typedef __uint8_t uint8_t;
typedef char gchar;
typedef void* gpointer;

extern void g_free(gpointer mem);
extern gchar* g_strdup(const gchar* str) __attribute__((__malloc__));

static inline void
g_autoptr_cleanup_generic_gfree(void* p)
{
  void** pp = (void**)p;
  g_free(*pp); /* { dg-bogus "use of uninitialized value" } */
}

typedef struct Object Object;

void
error_setg_internal(const char* fmt,
		    ...) __attribute__((__format__(gnu_printf, 1, 2)));
void
visit_type_str(const char* name, char** obj);
typedef struct SpaprMachineState SpaprMachineState;

extern uint8_t
spapr_get_cap(SpaprMachineState* spapr, int cap);

typedef struct SpaprCapPossible
{
  int num;
  /* [...snip...] */
  const char* vals[];
} SpaprCapPossible;

typedef struct SpaprCapabilityInfo
{
  const char* name;
  /* [...snip...] */
  int index;
  /* [...snip...] */
  SpaprCapPossible* possible;
  /* [...snip...] */
} SpaprCapabilityInfo;

void
spapr_cap_get_string(SpaprMachineState* spapr,
		     const char* name,
		     SpaprCapabilityInfo* cap)
{
  __attribute__((cleanup(g_autoptr_cleanup_generic_gfree))) char* val = NULL;
  uint8_t value = spapr_get_cap(spapr, cap->index);

  if (value >= cap->possible->num) {
    error_setg_internal("Invalid value (%d) for cap-%s",
			value,
			cap->name);
    return;
  }

  val = g_strdup(cap->possible->vals[value]);

  visit_type_str(name, &val);
}
