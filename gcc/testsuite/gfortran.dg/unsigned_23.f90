! { dg-do run }
! { dg-options "-funsigned" }
! Test some functionality for SELECT
program main
  implicit none
  integer :: i
  unsigned :: u
  logical, dimension(-3:3) :: seen
  seen = .false.
  do i=-3,3
     u = uint(i)
     select case(u)
     case (4294967293u)
        if (seen(i)) error stop 1
        seen(i) = .true.
     case (4294967294u)
        if (seen(i)) error stop 2
        seen(i) = .true.
     case (4294967295u)
        if (seen(i)) error stop 3
        seen(i) = .true.
     case (0u)
        if (seen(i)) error stop 4
        seen(i) = .true.
     case (1u)
        if (seen(i)) error stop 5
        seen(i) = .true.
     case (2u)
        if (seen(i)) error stop 6
        seen(i) = .true.
     case (3u)
        if (seen(i)) error stop 7
        seen(i) = .true.        
     case default
        error stop 8
     end select
  end do
  if (any(.not.seen)) error stop 9
end program main
