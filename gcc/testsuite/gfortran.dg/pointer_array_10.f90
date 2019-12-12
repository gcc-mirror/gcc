! { dg-do run }
!
! Test the fix for PR87336, in which the 'span' field of the array
! descriptor, passed to 'show', was not set.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de> following
! a posting to clf by 'Spectrum'.
!
program main
  implicit none
  integer, target :: a( 2:4 )

  a = [2,3,4]
!  print *, "a [before] = ", a
  call show( a )
!  print *, "a [after]  = ", a
  if (any (a .ne. [200,300,400])) stop 1

contains
  subroutine show( arr )
    integer, pointer, intent(in) :: arr(:)
!    print *, "arr = ", arr
!    print *, "bounds = ", lbound(arr), ubound(arr)
    arr(:) = [200,300,400]
!    print *, "arr2= ", arr
  end subroutine show
  end program
