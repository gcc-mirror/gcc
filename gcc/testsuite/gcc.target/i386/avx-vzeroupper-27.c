/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mtune=generic -dp" } */

typedef struct objc_class *Class;
typedef struct objc_object
{
  Class class_pointer;
} *id;

typedef const struct objc_selector *SEL;
typedef void * retval_t;
typedef void * arglist_t;

extern retval_t __objc_forward (id object, SEL sel, arglist_t args);

double
__objc_double_forward (id rcv, SEL op, ...)
{
  void *args, *res;

  args = __builtin_apply_args ();
  res = __objc_forward (rcv, op, args);
  __builtin_return (res);
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 2 } } */
