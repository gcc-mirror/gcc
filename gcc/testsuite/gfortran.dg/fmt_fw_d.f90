! { dg-do run }
! { dg-options "-std=gnu" }
! PR47567 Wrong output for small absolute values with F editing
! Test case provided by Thomas Henlich 
call verify_fmt(1.2)
call verify_fmt(-0.1)
call verify_fmt(1e-7)
call verify_fmt(1e-6)
call verify_fmt(1e-5)
call verify_fmt(1e-4)
call verify_fmt(1e-3)
call verify_fmt(1e-2)
call verify_fmt(-1e-7)
call verify_fmt(-1e-6)
call verify_fmt(-1e-5)
call verify_fmt(-1e-4)
call verify_fmt(-1e-3)
call verify_fmt(-1e-2)
call verify_fmt(tiny(0.0))
call verify_fmt(-tiny(0.0))
call verify_fmt(0.0)
call verify_fmt(-0.0)
call verify_fmt(100.0)
call verify_fmt(.12345)
call verify_fmt(1.2345)
call verify_fmt(12.345)
call verify_fmt(123.45)
call verify_fmt(1234.5)
call verify_fmt(12345.6)
call verify_fmt(123456.7)
call verify_fmt(99.999)
call verify_fmt(-100.0)
call verify_fmt(-99.999)
end

! loop through values for w, d
subroutine verify_fmt(x)
    real, intent(in) :: x
    integer :: w, d
    character(len=80) :: str, str0
    integer :: len, len0
    character(len=80) :: fmt_w_d
    logical :: result, have_num, verify_fmt_w_d
    
    do d = 0, 10
        have_num = .false.
        do w = 1, 20
            str = fmt_w_d(x, w, d)
            len = len_trim(str)
            
            result = verify_fmt_w_d(x, str, len, w, d)
            if (.not. have_num .and. result) then
                have_num = .true.
                str0 = fmt_w_d(x, 0, d)
                len0 = len_trim(str0)
                if (len /= len0) then
                    call errormsg(x, str0, len0, 0, d, "selected width is wrong")
                else
                    if (str(:len) /= str0(:len0)) call errormsg(x, str0, len0, 0, d, "output is wrong")
                end if
            end if
        end do
    end do

end subroutine

! checks for standard-compliance, returns .true. if field contains number, .false. on overflow
function verify_fmt_w_d(x, str, len, w, d)
    real, intent(in) :: x
    character(len=80), intent(in) :: str
    integer, intent(in) :: len
    integer, intent(in) :: w, d
    logical :: verify_fmt_w_d
    integer :: pos
    character :: decimal_sep = "."

    verify_fmt_w_d = .false.
    
    ! check if string is all asterisks
    pos = verify(str(:len), "*")
    if (pos == 0) return
    
    ! check if string contains a digit
    pos = scan(str(:len), "0123456789")
    if (pos == 0) call errormsg(x, str, len, w, d, "no digits")

    ! contains decimal separator?
    pos = index(str(:len), decimal_sep)
    if (pos == 0) call errormsg(x, str, len, w, d, "no decimal separator")
    
    ! negative and starts with minus?
    if (sign(1., x) < 0.) then
        pos = verify(str, " ")
        if (pos == 0) call errormsg(x, str, len, w, d, "only spaces")
        if (str(pos:pos) /= "-") call errormsg(x, str, len, w, d, "no minus sign")
    end if
    
    verify_fmt_w_d = .true.
end function

function fmt_w_d(x, w, d)
    real, intent(in) :: x
    integer, intent(in) :: w, d
    character(len=*) :: fmt_w_d
    character(len=10) :: fmt, make_fmt
    
    fmt = make_fmt(w, d)
    write (fmt_w_d, fmt) x
end function

function make_fmt(w, d)
    integer, intent(in) :: w, d
    character(len=10) :: make_fmt
    
    write (make_fmt,'("(f",i0,".",i0,")")') w, d
end function

subroutine errormsg(x, str, len, w, d, reason)
    real, intent(in) :: x
    character(len=80), intent(in) :: str
    integer, intent(in) :: len, w, d
    character(len=*), intent(in) :: reason
    integer :: fmt_len
    character(len=10) :: fmt, make_fmt
    
    fmt = make_fmt(w, d)
    fmt_len = len_trim(fmt)
    
    !print *, "print '", fmt(:fmt_len), "', ", x, " ! => ", str(:len), ": ", reason
    STOP 1
end subroutine
