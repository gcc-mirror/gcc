! { dg-do run }
program char_pointer_func
! Test assignments from character pointer functions, required
! to fix PR17192 and PR17202
! Provided by Paul Thomas pault@gcc.gnu.org
  implicit none
  character*4                :: c0
  character*4, pointer       :: c1
  character*4, pointer       :: c2(:)
  allocate (c1, c2(1))
! Check that we have not broken non-pointer characters.
  c0 = foo ()
  if (c0 /= "abcd") call abort ()
! Value assignments
  c1 = sfoo ()
  if (c1 /= "abcd") call abort ()
  c2 = afoo (c0)
  if (c2(1) /= "abcd") call abort ()
  deallocate (c1, c2)
! Pointer assignments
  c1 => sfoo ()
  if (c1 /= "abcd") call abort ()
  c2 => afoo (c0)
  if (c2(1) /= "abcd") call abort ()
  deallocate (c1, c2)
contains
  function foo () result (cc1)
    character*4                :: cc1
    cc1 = "abcd"
  end function foo
  function sfoo () result (sc1)
    character*4, pointer       :: sc1
    allocate (sc1)
    sc1 = "abcd"
  end function sfoo
  function afoo (c0) result (ac1)
    character*4                :: c0
    character*4, pointer       :: ac1(:)
    allocate (ac1(1))
    ac1 = "abcd"
  end function afoo
end program char_pointer_func
