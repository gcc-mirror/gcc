/* Copyright (C) 2000 Free Software Foundation */
/* by Alexandre Oliva  <aoliva@redhat.com> */

void abort (void);
void exit (int);

enum foo { FOO, BAR };

/* Even though the underlying type of an enum is unspecified, the type
   of enumeration constants is explicitly defined as int (6.4.4.3/2 in
   the C99 Standard).  Therefore, `i' must not be promoted to
   `unsigned' in the comparison below; we must exit the loop when it
   becomes negative. */

int
main ()
{
  int i;
  for (i = BAR; i >= FOO; --i)
    if (i == -1)
      abort ();

  exit (0);
}

