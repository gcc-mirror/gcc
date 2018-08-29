! { dg-do compile }
! PR Fortran/82367
! Contributed by Walter Spector <w6ws at earthlink dot net>
module cls_allocmod
  implicit none

contains

 subroutine cls_alloc (n, str)
    integer,  intent(in) :: n
    character(*), allocatable, intent(out) :: str
!  Note: Star ^ should have been a colon (:)

    allocate (character(n)::str)

  end subroutine

end module

program cls
  use cls_allocmod
  implicit none

  character(:), allocatable :: s

  call cls_alloc(42, s) ! { dg-error "allocatable or pointer dummy argument" }
  print *, 'string len =', len(s)

end program
