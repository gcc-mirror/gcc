! { dg-do compile }
!
! PR fortran/83522
!
! Contributed by urbanjost and Jerry DeLisle
!
program testit
  character(len=:),allocatable :: strings(:)
  integer :: i
  strings=[character(len=2) :: 'AA','BB']
  write(*,*)strings(:)(:)  ! { dg-error "Substring reference of nonscalar not permitted" }
  !strings(:)(:) ! Parse error: "Invalid character in name"
  strings(:)(:) = 'x'   ! { dg-error "Substring reference of nonscalar not permitted" }
  do i=1, size(strings)
    write(*,*)strings(i)(:)  ! This is valid and works
  end do
end program testit
