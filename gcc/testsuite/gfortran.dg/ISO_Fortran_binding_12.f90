! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_12.c }
!
! Test the fix for PR90093. The additional source is the main program.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_optional
  use, intrinsic :: iso_c_binding
  implicit none
  integer :: status = 0

contains

  subroutine foo_opt(this, that, sz, flag) bind(c)
    real(c_float), optional :: this(:)
    real(c_float), optional :: that(*)
    integer(c_int), optional :: sz
    integer(c_int), value :: flag
    if (flag == 0) then
       if (.not. present(this) .or. present(that) .or. present(sz)) then
          write(*,*) 'FAIL 1', present(this), present(that), present(sz)
          status = status + 1
       end if
    else if (flag == 1) then
       if (present(this) .or. .not. present(that) .or. .not. present(sz)) then
          write(*,*) 'FAIL 2', present(this), present(that), present(sz)
          status = status + 1
       end if
       if (sz /= 12) then
          write(*,*) 'FAIL 3'
          status = status + 1
       end if
    else if (flag == 2) then
       if (present(this) .or. present(that) .or. present(sz)) then
          write(*,*) 'FAIL 4', present(this), present(that), present(sz)
          status = status + 1
       end if
    end if
  end subroutine foo_opt

  subroutine write_res() BIND(C)
! Add a check that the fortran missing optional is accepted by the
! bind(C) procedure.
    call foo_opt (flag = 2)
    if (status == 0) then
       write(*,*) 'OK'
    else
       stop 1
    end if
  end subroutine

end module mod_optional
