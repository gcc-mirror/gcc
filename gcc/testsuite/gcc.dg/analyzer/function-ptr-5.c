#define NULL ((void *)0)

void calling_null_fn_ptr_1 (void)
{
  void (*fn_ptr) (void) = NULL;
  fn_ptr (); /* { dg-warning "jump through null pointer" } */
}

int calling_null_fn_ptr_2 (void)
{
  int (*fn_ptr) (void) = NULL;
  return fn_ptr (); /* { dg-warning "jump through null pointer" } */
}

typedef void (*void_void_fn_ptr) (void);

void calling_const_fn_ptr (void)
{
  void_void_fn_ptr fn_ptr = (void_void_fn_ptr)0xffd2;
  return fn_ptr ();
}

void skipping_init (int flag)
{
  void_void_fn_ptr fn_ptr = NULL;
  if (flag) /* { dg-message "branch" } */
    fn_ptr = (void_void_fn_ptr)0xffd2;
  fn_ptr (); /* { dg-warning "jump through null pointer" } */
}

struct callbacks
{
  void_void_fn_ptr on_redraw;
  void_void_fn_ptr on_cleanup;  
};

void test_callbacks (void)
{
  struct callbacks cb;
  __builtin_memset (&cb, 0, sizeof (cb));
  cb.on_cleanup (); /* { dg-warning "jump through null pointer" } */
}
