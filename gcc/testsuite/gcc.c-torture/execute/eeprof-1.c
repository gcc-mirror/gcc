#define ASSERT(X)	if (!(X)) abort ();
#define NOCHK __attribute__ ((no_instrument_function))

int entry_calls, exit_calls;
void (*last_fn_entered)();
void (*last_fn_exited)();

int main () NOCHK;

void foo ()
{
  ASSERT (last_fn_entered == foo);
}

static void foo2 ()
{
  ASSERT (entry_calls == 1 && exit_calls == 0);
  ASSERT (last_fn_entered == foo2);
  foo ();
  ASSERT (entry_calls == 2 && exit_calls == 1);
  ASSERT (last_fn_entered == foo);
  ASSERT (last_fn_exited == foo);
}

void nfoo (void) NOCHK;
void nfoo ()
{
  ASSERT (entry_calls == 2 && exit_calls == 2);
  ASSERT (last_fn_entered == foo);
  ASSERT (last_fn_exited == foo2);
  foo ();
  ASSERT (entry_calls == 3 && exit_calls == 3);
  ASSERT (last_fn_entered == foo);
  ASSERT (last_fn_exited == foo);
}

int main ()
{
  ASSERT (entry_calls == 0 && exit_calls == 0);

  foo2 ();

  ASSERT (entry_calls == 2 && exit_calls == 2);
  ASSERT (last_fn_entered == foo);
  ASSERT (last_fn_exited == foo2);

  nfoo ();

  ASSERT (entry_calls == 3 && exit_calls == 3);
  ASSERT (last_fn_entered == foo);

  return 0;
}

void __cyg_profile_func_enter (void (*fn)(), void (*parent)()) NOCHK;
void __cyg_profile_func_exit (void (*fn)(), void (*parent)()) NOCHK;

void __cyg_profile_func_enter (void (*fn)(), void (*parent)())
{
  entry_calls++;
  last_fn_entered = fn;
}
void __cyg_profile_func_exit (void (*fn)(), void (*parent)())
{
  exit_calls++;
  last_fn_exited = fn;
}
