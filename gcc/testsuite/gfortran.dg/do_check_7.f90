! { dg-do compile }
! PR 30146 - warn about DO variables as argument to INTENT(IN) and
! INTENT(INOUT) dummy arguments
program main
  implicit none
  integer :: i,j, k, l
  do k=1,2                      ! { dg-error "undefined value" }
     do i=1,10                  ! { dg-error "definable" }
        do j=1,10               ! { dg-error "undefined value" }
           do l=1,10            ! { dg-error "definable" }
              call s_out(k)      ! { dg-error "undefined" }
              call s_inout(i)    ! { dg-error "definable" }
              print *,f_out(j)   ! { dg-error "undefined" }
              print *,f_inout(l) ! { dg-error "definable" }
           end do
        end do
     end do
  end do
contains
  subroutine s_out(i_arg)
    integer, intent(out) :: i_arg
  end subroutine s_out

  subroutine s_inout(i_arg)
    integer, intent(inout) :: i_arg
  end subroutine s_inout

  function f_out(i_arg)
    integer, intent(out) :: i_arg
    integer :: f_out
    f_out = i_arg
  end function f_out

  function f_inout(i_arg)
    integer, intent(inout) :: i_arg
    integer :: f_inout
    f_inout = i_arg
  end function f_inout

end program main
