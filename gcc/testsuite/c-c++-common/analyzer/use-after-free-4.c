#include "analyzer-decls.h"

struct coord
{
  int x;
  int y;
};

/* 'free' of a pointer parameter.  */

int read_x_after_free_of_ptr_param (struct coord *c)
{
  __builtin_free (c); /* { dg-message "freed here" } */
  return c->x; /* { dg-warning "use after 'free' of 'c'" } */
}

int read_x_after_free_of_ptr_param_with_cast (void *p)
{
  struct coord *c = (struct coord *)p;
  __builtin_free (p); /* { dg-message "freed here" } */
  return c->x; /* { dg-warning "use after 'free' of 'c'" } */
}

int read_y_after_free_of_ptr_param (struct coord *c)
{
  __builtin_free (c); /* { dg-message "freed here" } */
  return c->y; /* { dg-warning "use after 'free' of 'c'" } */
}

void write_x_after_free_of_ptr_param (struct coord *c)
{
  __builtin_free (c); /* { dg-message "freed here" } */
  c->x = 42; /* { dg-warning "use after 'free' of 'c'" } */
}

void write_y_after_free_of_ptr_param (struct coord *c)
{
  __builtin_free (c); /* { dg-message "freed here" } */
  c->y = 42; /* { dg-warning "use after 'free' of 'c'" } */
}

void write_y_after_free_of_ptr_param_with_cast (void *p)
{
  struct coord *c = (struct coord *)p;
  __builtin_free (p); /* { dg-message "freed here" } */
  c->y = 42; /* { dg-warning "use after 'free' of 'c'" } */
}

/* 'free' of a 'malloc'ed buffer.  */

int read_x_after_free_of_malloc_buffer (void)
{
  struct coord *c = (struct coord *)__builtin_malloc (sizeof (struct coord));
  if (!c)
    return 0;
  __builtin_free (c); /* { dg-message "freed here" } */
  return c->x; /* { dg-warning "use after 'free' of 'c'" } */
}

int read_y_after_free_of_malloc_buffer (void)
{
  struct coord *c = (struct coord *)__builtin_malloc (sizeof (struct coord));
  if (!c)
    return 0;
  __builtin_free (c); /* { dg-message "freed here" } */
  return c->y; /* { dg-warning "use after 'free' of 'c'" } */
}

void write_x_after_free_of_malloc_buffer (void)
{
  struct coord *c = (struct coord *)__builtin_malloc (sizeof (struct coord));
  if (!c)
    return;
  __builtin_free (c); /* { dg-message "freed here" } */
  c->x = 42; /* { dg-warning "use after 'free' of 'c'" } */
}

void write_y_after_free_of_malloc_buffer (void)
{
  struct coord *c = (struct coord *)__builtin_malloc (sizeof (struct coord));
  if (!c)
    return;
  __builtin_free (c); /* { dg-message "freed here" } */
  c->y = 42; /* { dg-warning "use after 'free' of 'c'" } */
}
