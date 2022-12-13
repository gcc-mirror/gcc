! { dg-do run }
! PR fortran/107444
!
! Check that procedures with optional arguments that have the value attribute
! work for intrinsic types including character, and that the presence check
! works.
!
! Co-contributed by M.Morin

program p
  implicit none
  interface
     subroutine i(c, o)
       character(*) :: c
       character(3), optional, value :: o
     end subroutine i
  end interface
  procedure(i), pointer :: pp
  call s([.false.,.false.,.false.],  0)
  call s([.true., .false.,.false.], 10, i=7)
  call s([.false.,.true. ,.false.], 20, c='abc')
  call s([.false.,.false.,.true. ], 30, r=3.0)
  pp => f
  call pp ("abcd", "xyz")
contains
  subroutine s (expect,code,i,c,r)
    logical, intent(in)           :: expect(:)
    integer, intent(in)           :: code
    integer     , value, optional :: i
    character(3), value, optional :: c
    real        , value, optional :: r
    if (expect(1) .neqv. present (i)) stop 1+code
    if (expect(2) .neqv. present (c)) stop 2+code
    if (expect(3) .neqv. present (r)) stop 3+code
    if (present (i)) then
       if (i /= 7) stop 4+code
    end if
    if (present (c)) then
       if (c /= "abc") stop 5+code
    end if
    if (present (r)) then
       if (r /= 3.0) stop 6+code
    end if
  end subroutine s
  subroutine f (c, o)
    character(*) :: c
    character(3), optional, value :: o
    if (c /= "abcd") stop 41
    if (len (c) /= 4) stop 42
    if (.not. present (o)) stop 43
    if (o /= "xyz")  stop 44
  end subroutine f
end
