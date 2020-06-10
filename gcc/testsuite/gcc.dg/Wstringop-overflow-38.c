/* Verify that non-constant offsets don't suppress warnings in cases
   when the size of the access alone makes it invalid, regardless of
   the offset value.
   Also verify that writing into unknown objects through pointers
   (or pointer members) doesn't trigger warnings.
   { dg-do compile }
   { dg-options "-O1 -Wall -Wno-array-bounds" } */

typedef __SIZE_TYPE__ size_t;

extern void* memcpy (void*, const void*, size_t);
extern void* memset (void*, int, size_t);


struct Xp { char *p; } xp;
struct Xa { char *a[2]; } xa;

void nowarn_copy_read_pointed_obj_plus_cst (void *d)
{
  int i = 0;
  memcpy (d, xp.p + i, 9);
  memcpy (d, xa.a[i], 9);
}

void nowarn_copy_read_pointed_obj_plus_var (void *d, int i)
{
  memcpy (d, xp.p + i, 9);
  memcpy (d, xa.a[i], 9);
}


void warn_copy_read_pointer_plus_cst (void *d)
{
  int i = 0;
  memcpy (d, &xp.p + i, 9);   // { dg-warning "reading 9 bytes from a region of size . " }
}

void warn_copy_read_pointer_plus_var (void *d, int i)
{
  memcpy (d, &xp.p + i, 9);   // { dg-warning "reading 9 bytes from a region of size . " }
}


void nowarn_copy_write_pointed_obj_plus_cst (const void *s)
{
  int i = 0;
  memcpy (xp.p + i, s, 9);
  memcpy (xa.a[i], s, 9);
}

void nowarn_copy_write_pointed_obj_plus_var (const void *s, int i)
{
  memcpy (xp.p + i, s, 9);
  memcpy (xa.a[i], s, 9);
}


void warn_copy_write_pointer_plus_cst (const void *s)
{
  int i = 0;
  memcpy (&xp.p + i, s, 9);   // { dg-warning "writing 9 bytes into a region of size . " }
}

void warn_copy_write_pointer_plus_var (const void *s, int i)
{
  memcpy (&xp.p + i, s, 9);   // { dg-warning "writing 9 bytes into a region of size . " }
}


void nowarn_set_pointed_obj_plus_cst (void)
{
  int i = 0;
  memset (xp.p + i, 0, 9);
  memset (xa.a[i], 0, 9);
}

void nowarn_set_pointed_obj_plus_var (int i)
{
  memset (xp.p + i, 0, 9);
  memset (xa.a[i], 0, 9);
}


void warn_set_pointer_plus_cst (void)
{
  int i = 0;
  memset (&xp.p + i, 0, 9);   // { dg-warning "writing 9 bytes into a region of size . " }
}

void warn_set_pointer_plus_var (int i)
{
  memset (&xp.p + i, 0, 9);   // { dg-warning "writing 9 bytes into a region of size . " }
  memset (&xa.a[i], 0, 17);   // { dg-warning "writing 17 bytes into a region of size \[0-9\]+ " }
}
