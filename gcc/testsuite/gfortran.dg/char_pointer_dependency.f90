! { dg-do run }
! Test assignments from character pointer functions with dependencies
! are correctly resolved.
! Provided by Paul Thomas pault@gcc.gnu.org
program char_pointer_dependency
  implicit none
  character*4, pointer       :: c2(:)
  allocate (c2(2))
  c2 = (/"abcd","efgh"/)
  c2 = afoo (c2)
  if (c2(1) /= "efgh") call abort ()
  if (c2(2) /= "abcd") call abort ()
  deallocate (c2)
contains
  function afoo (ac0) result (ac1)
    integer                    :: j
    character*4                :: ac0(:)
    character*4, pointer       :: ac1(:)
    allocate (ac1(2))
    do j = 1,2
      ac1(j) = ac0(3-j)
    end do
  end function afoo
end program char_pointer_dependency
