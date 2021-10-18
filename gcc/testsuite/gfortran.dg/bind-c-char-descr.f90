! PR fortran/92482
!
! Contributed by José Rui Faustino de Sousa 
!
! Note the xfail issue below for 'strg_print_2("abc")

program strp_p

  use, intrinsic :: iso_c_binding, only: &
    c_char
    
  implicit none

  integer, parameter :: l = 3

  character(len=l, kind=c_char),  target :: str
  character(len=:, kind=c_char), pointer :: strp_1
  character(len=l, kind=c_char), pointer :: strp_2

  str = "abc"
  nullify(strp_1, strp_2)
  strp_1 => str
  strp_2 => str
  if (len(str) /= 3 .or. str /= "abc") stop 1
  if (len(strp_1) /= 3 .or. strp_1 /= "abc") stop 2
  if (len(strp_2) /= 3 .or. strp_2 /= "abc") stop 3
  call strg_print_0("abc") ! Error (10.0.0) or segmentation fault (9.1.0)
  call strg_print_0(str) ! Error (10.0.0) or segmentation fault (9.1.0)
  call strg_print_0(strp_1) ! Error (10.0.0) or segmentation fault (9.1.0)
  call strg_print_0(strp_2) ! Error (10.0.0) or segmentation fault (9.1.0)
  call strg_print_1(strp_1) ! Not yet supported

  call strg_print_2("abc", xfail=.true.)
  call strg_print_2(str)
  call strg_print_2(strp_1)
  call strg_print_2(strp_2)

  call strg_print_2_c("abc")
  call strg_print_2_c(str)
  call strg_print_2_c(strp_1)
  call strg_print_2_c(strp_2)

contains

  subroutine strg_print_0(this) bind(c) ! Error (10.0.0 20191106) or warning (9.1.0) issued with bind(c)
    character(len=*, kind=c_char), target, intent(in) :: this

    if (len (this) /= 3) stop 10
    if (this /= "abc") stop 11
  end subroutine strg_print_0
  
  subroutine strg_print_1(this) bind(c) ! Not yet supported with bind(c)
    character(len=:, kind=c_char), pointer, intent(in) :: this
    character(len=:), pointer :: strn

    if (.not. associated (this)) stop 20
    if (len (this) /= 3) stop 21
    if (this /= "abc") stop 22
     strn => this
     if (.not. associated (strn)) stop 23
     if(associated(strn))then
       if (len (this) /= 3) stop 24
       if (this /= "abc") stop 25
     end if
   end subroutine strg_print_1
  
  subroutine strg_print_2(this, xfail)
    use, intrinsic :: iso_c_binding, only: &
      c_loc, c_f_pointer
    
    type(*), target, intent(in) :: this(..)
    logical, optional, value :: xfail
    character(len=l), pointer :: strn

    call c_f_pointer(c_loc(this), strn)
    if (.not. associated (strn)) stop 30
    if(associated(strn))then
      if (len (strn) /= 3) stop 31
      if (strn /= "abc") then
        if (present (xfail)) then
          print *, 'INVALID STRING - EXPECTED "abc" / PR47225'
        else
          stop 32
        end if
      end if
    end if
  end subroutine strg_print_2

  subroutine strg_print_2_c(this) bind(c)
    use, intrinsic :: iso_c_binding, only: &
      c_loc, c_f_pointer
    
    type(*), target, intent(in) :: this(..)
    character(len=l), pointer :: strn

    call c_f_pointer(c_loc(this), strn)
    if (.not. associated (strn)) stop 40
    if(associated(strn))then
      if (len (strn) /= 3) stop 41
      if (strn /= "abc") stop 42
    end if
  end subroutine strg_print_2_c

end program strp_p
