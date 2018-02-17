! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test passing STRUCTUREs through functions and subroutines.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

module dec_structure_7m
  structure /s1/
    integer i1
    logical l1
    real r1
    character c1
  end structure

  structure /s2/
    integer i
    record /s1/ r1
  endstructure

contains
  ! Pass structure through subroutine
  subroutine sub (rec1, i)
    implicit none
    integer, intent(in) :: i
    record /s1/ rec1
    rec1.i1 = i
  end subroutine

  ! Pass structure through function
  function func (rec2, r)
    implicit none
    real, intent(in) :: r
    record /s2/ rec2
    real func
    rec2.r1.r1 = r
    func = rec2.r1.r1
    return
  end function
end module

program dec_structure_7
  use dec_structure_7m

  implicit none
  record /s1/ r1
  record /s2/ r2
  real junk

  ! Passing through functions and subroutines
  r1.i1 = 0
  call sub (r1, 10)

  r2.r1.r1 = 0.0
  junk = func (r2, -20.14)

  if (r1.i1 .ne. 10) then
    call aborts("sub(r1, 10)")
  endif

  if (r2.r1.r1 .ne. -20.14) then
    call aborts("func(r2, -20.14)")
  endif

  if (junk .ne. -20.14) then
    print *, junk
    call aborts("junk = func()")
  endif

end program
