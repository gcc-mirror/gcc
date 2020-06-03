! { dg-do run }
!
! PR fortran/95214
!

program chr_p

  implicit none

  integer, parameter :: u = 65
  
  integer, parameter :: n = 26
  
  character :: c(n)
  integer   :: i

  c = [(achar(i), i=u,u+n-1)]
  call chr_s(c, c)
  call gfc_descriptor_c_char(c)
  call s1(c)
  call s1s_a(c)
  call s1s_b(c)
  call s2(c)
  stop
  
contains

  subroutine chr_s(a, b)
    character, intent(in) :: a(..)
    character, intent(in) :: b(:)

    integer :: i

    select rank(a)
    rank(1)
      do i = 1, size(a)
        if(a(i)/=b(i)) stop 1
      end do
    rank default
      stop 2
    end select
    return
  end subroutine chr_s

  ! From Bug 66833
  ! Contributed by Damian Rouson <damian@sourceryinstitute.org>
  subroutine gfc_descriptor_c_char(a)
    character a(..)
    if(rank(a)/=1) stop 3 ! ICE (also for lbound, ubound, and c_loc)
  end subroutine gfc_descriptor_c_char


  ! From Bug 67938
  ! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>
  
  ! example z1.f90
  subroutine s1(x)
    character(1) :: x(..)
    if(any(lbound(x)/=[1])) stop 4
    if(any(ubound(x)/=[n])) stop 5
  end subroutine s1
  
  ! example z1s.f90
  subroutine s1s_a(x)
    character :: x(..)
    if(size(x)/=n) stop 6
  end subroutine s1s_a
  
  subroutine s1s_b(x)
    character(77) :: x(..)
    if(size(x)/=n) stop 7
  end subroutine s1s_b
  
  ! example z2.f90
  subroutine s2(x)
    character(1) :: x(..)
    if(lbound(x, dim=1)/=1) stop 8
    if(ubound(x, dim=1)/=n) stop 9
    if(size(x, dim=1)/=n)   stop 10
  end subroutine s2
  
end program chr_p


