typedef unsigned int size_t;
typedef const struct objc_selector
{
  void *sel_id;
}
 *SEL;
typedef struct objc_object
{
}
 *id;
typedef struct objc_class *Class;
struct objc_class
{
  struct sarray *dtable;
};
typedef size_t sidx;
struct soffset
{
  unsigned int boffset:(sizeof (size_t) * 8) / 2;
  unsigned int eoffset:(sizeof (size_t) * 8) / 2;
};
union sofftype
{
  struct soffset off;
  sidx idx;
};
struct sarray
{
  size_t capacity;
};
static __inline__ unsigned int
soffset_decode (sidx indx)
{
  union sofftype x;
  x.idx = indx;
  return x.off.eoffset + (x.off.boffset * (1 << 5));
}
static __inline__ void *
sarray_get_safe (struct sarray *array, sidx indx)
{
  if (soffset_decode (indx) < array->capacity)
    return (void *)sarray_get (array, indx);
}
void *
get_imp (Class class, SEL sel)
{
  void *res = sarray_get_safe (class->dtable, (size_t) sel->sel_id);
  if (res == 0)
    {
	{
	  res = get_imp (class, sel);
	}
    }
}
