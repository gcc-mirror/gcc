/* Tests for #pragma init and #pragma fini.  */

/* { dg-do run { target *-*-solaris2.* } } */

extern void abort ();

#pragma init		/* { dg-error "malformed" } */
#pragma init ()		/* { dg-error "malformed" } */
#pragma init init_func	/* { dg-error "malformed" } */

#pragma fini		/* { dg-error "malformed" } */
#pragma fini ()		/* { dg-error "malformed" } */
#pragma fini fini_func	/* { dg-error "malformed" } */

#pragma init (init_func, init_static_func)

int glob_1, glob_2;

void init_func (void)
{
  glob_1 = 1;
}

static void init_static_func (void)
{
  glob_2 = 2;
}

#pragma fini (fini_func, fini_static_func)

void fini_func (void)
{

}

static void fini_static_func (void)
{

}

int main()
{
  if (glob_1 != 1)
    abort ();

  if (glob_2 != 2)
    abort ();

  return 0;
}
