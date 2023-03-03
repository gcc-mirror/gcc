/* Exercise basic C++-only cases of -Wdangling-pointer.
   { dg-do compile }
   { dg-options "-Wall -Wno-class-memaccess" } */

extern "C" void* memset (void*, int, __SIZE_TYPE__);

void sink (const void*, ...);

struct S { S (); };

void nowarn_int_ref (int i)
{
  const S &sref = S ();
  const int &iref = 1 + i;
  sink (&sref, &iref);
}

void warn_init_ref_member ()
{
  struct AS
  {
    const S &sref;
    AS ():
      // The temporary S object is destroyed when AS::AS() returns.
      sref (S ())  // { dg-warning "storing the address" }
    { }
  } as;

  struct Ai
  {
    const int &iref;
    Ai ():
      // The temporary int is destroyed when Ai::Ai() returns.
      iref (1 + 1)  // { dg-warning "storing the address" }
    { }
  } ai;

  struct Al
  {
    const S &sref;
    Al ():
      // The temporary S object is destroyed when Al::Al() returns.
      sref (S ())  // { dg-warning "storing the address" }
    {
      // Copying this to an SSA_NAME used to disable the warning:
      Al *ptr = this;
      asm ("" : "+r" (ptr));
    }
  } al;

  struct An
  {
    An *next;
    const S &sref;
    An ():
      next (0),
      // The temporary S object is destroyed when An::An() returns.
      sref (S ())  // { dg-warning "storing the address" "" { xfail *-*-* } }
    {
      // ??? Writing to another part of *this disables the warning:
      next = 0;
    }
  } an;

  sink (&as, &ai, &al, &an);
}


void default_ref_arg (const S& = S ());

void nowarn_call_default_ref_arg ()
{
  default_ref_arg ();
}


void nowarn_array_access ()
{
  /* Verify that the clobber in the exceptional basic block doesn't
     cause bogus warnings.  */
  S a[1];
  memset (a, 0, sizeof a);
  sink (a);
}


void nowarn_array_access_cond (int i)
{
  if (i)
    {
      S a1[1];
      memset (a1, 0, sizeof a1);
      sink (a1);
    }
  else
    {
      S a2[2];
      memset (a2, 0, sizeof a2);
      sink (a2);
    }
}
