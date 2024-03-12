! { dg-do run }
! PR fortran/110825 - TYPE(*) and character actual arguments

program foo
  use iso_c_binding, only: c_loc, c_ptr, c_associated
  implicit none
  character(100)            :: not_used = ""
  character(:), allocatable :: deferred
  character                 :: c42(6,7) = "*"
  call sub  (not_used,  "123")
  call sub  ("0"      , "123")
  deferred = "d"
  call sub  (deferred , "123")
  call sub2 ([1.0,2.0], "123")
  call sub2 (["1","2"], "123")
  call sub3 (c42      , "123")

contains

  subroutine sub (useless_var, print_this)
    type(*),      intent(in) :: useless_var
    character(*), intent(in) :: print_this
    if (len      (print_this) /= 3) stop 1
    if (len_trim (print_this) /= 3) stop 2
  end

  subroutine sub2 (a, c)
    type(*),      intent(in) :: a(:)
    character(*), intent(in) :: c
    if (len      (c) /= 3) stop 10
    if (len_trim (c) /= 3) stop 11
    if (size (a)     /= 2) stop 12
  end

  subroutine sub3 (a, c)
    type(*),      intent(in), target, optional :: a(..)
    character(*), intent(in)                   :: c
    type(c_ptr) :: cpt
    if (len      (c) /= 3) stop 20
    if (len_trim (c) /= 3) stop 21
    if (.not. present (a)) stop 22
    if (rank (a)     /= 2) stop 23
    if (size (a)    /= 42) stop 24
    if (any (shape  (a) /= [6,7])) stop 25
    if (any (lbound (a) /= [1,1])) stop 26
    if (any (ubound (a) /= [6,7])) stop 27
    if (.not. is_contiguous (a))   stop 28
    cpt = c_loc (a)
    if (.not. c_associated (cpt))  stop 29
  end

end
