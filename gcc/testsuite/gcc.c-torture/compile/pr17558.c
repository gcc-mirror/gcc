struct xobject {
       char type;
};
extern struct xobject *t1_Xform ( struct xobject *obj);
struct xobject *
t1_Xform(struct xobject *obj)
{
  register struct font *F = (struct font *) obj;
  return((struct xobject*)F);
}
