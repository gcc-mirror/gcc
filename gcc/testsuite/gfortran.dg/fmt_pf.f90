! { dg-do run }
! PR70235 Incorrect output with PF format.
! Test case provided by Antoine Gardeux.
program pr70235
use ISO_FORTRAN_ENV
    implicit none
    integer, parameter :: j(size(real_kinds)+4)=[REAL_KINDS, [4, 4, 4, 4]]
    logical :: l_skip(4) = .false.
    integer :: i
    integer :: n_tst = 0, n_cnt = 0, n_skip = 0
    character(len=20) :: s, s1

!   Check that the default rounding mode is to nearest and to even on tie.
    do i=1,size(real_kinds)
      if (i == 1) then
        write(s, '(2F4.1,2F4.0)') real(-9.49999905,kind=j(1)), &
                                  real(9.49999905,kind=j(1)),  &
                                  real(9.5,kind=j(1)), real(8.5,kind=j(1))
        write(s1, '(3PE10.3,2PE10.3)') real(987350.,kind=j(1)), &
                                       real(98765.0,kind=j(1))
      else if (i == 2) then
        write(s, '(2F4.1,2F4.0)') real(-9.49999905,kind=j(2)), &
                                  real(9.49999905,kind=j(2)),  &
                                  real(9.5,kind=j(2)), real(8.5,kind=j(2))
        write(s1, '(3PE10.3,2PE10.3)') real(987350.,kind=j(2)), &
                                       real(98765.0,kind=j(2))
      else if (i == 3) then
        write(s, '(2F4.1,2F4.0)') real(-9.49999905,kind=j(3)), &
                                  real(9.49999905,kind=j(3)),  &
                                  real(9.5,kind=j(3)), real(8.5,kind=j(3))
        write(s1, '(3PE10.3,2PE10.3)') real(987350.,kind=j(3)), &
                                       real(98765.0,kind=j(3))
      else if (i == 4) then
        write(s, '(2F4.1,2F4.0)') real(-9.49999905,kind=j(4)), &
                                  real(9.49999905,kind=j(4)),  &
                                  real(9.5,kind=j(4)), real(8.5,kind=j(4))
        write(s1, '(3PE10.3,2PE10.3)') real(987350.,kind=j(4)), &
                                       real(98765.0,kind=j(4))
      end if
      if (s /= '-9.5 9.5 10.  8.' .or. s1 /= ' 987.4E+03 98.76E+03') then
        l_skip(i) = .true.
!        print "('Unsupported rounding for real(',i0,')')", j(i)
      end if
    end do
        

! Original test.
    call checkfmt("(-6PF8.3)", 1.0e4,    "   0.010")
    call checkfmt("(-6PF8.3)",   0.0,    "   0.000")

! Test for the bug in comment 6.
    call checkfmt("(-8pf18.3)", 643.125, "             0.000")
    call checkfmt("(-7pf18.3)", 643.125, "             0.000")
    call checkfmt("(-6pf18.3)", 643.125, "             0.001")
    call checkfmt("(-5pf18.3)", 643.125, "             0.006")
    call checkfmt("(-4pf18.3)", 643.125, "             0.064")
    call checkfmt("(-3pf18.3)", 643.125, "             0.643")
    call checkfmt("(-2pf18.3)", 643.125, "             6.431")
    call checkfmt("(-1pf18.3)", 643.125, "            64.312")
    call checkfmt("( 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(ru,-8pf18.3)", 643.125, "             0.001")
    call checkfmt("(ru,-7pf18.3)", 643.125, "             0.001")
    call checkfmt("(ru,-6pf18.3)", 643.125, "             0.001")
    call checkfmt("(ru,-5pf18.3)", 643.125, "             0.007")
    call checkfmt("(ru,-4pf18.3)", 643.125, "             0.065")
    call checkfmt("(ru,-3pf18.3)", 643.125, "             0.644")
    call checkfmt("(ru,-2pf18.3)", 643.125, "             6.432")
    call checkfmt("(ru,-1pf18.3)", 643.125, "            64.313")
    call checkfmt("(ru, 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(rd,-8pf18.3)", 643.125, "             0.000")
    call checkfmt("(rd,-7pf18.3)", 643.125, "             0.000")
    call checkfmt("(rd,-6pf18.3)", 643.125, "             0.000")
    call checkfmt("(rd,-5pf18.3)", 643.125, "             0.006")
    call checkfmt("(rd,-4pf18.3)", 643.125, "             0.064")
    call checkfmt("(rd,-3pf18.3)", 643.125, "             0.643")
    call checkfmt("(rd,-2pf18.3)", 643.125, "             6.431")
    call checkfmt("(rd,-1pf18.3)", 643.125, "            64.312")
    call checkfmt("(rd, 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(rz,-8pf18.3)", 643.125, "             0.000")
    call checkfmt("(rz,-7pf18.3)", 643.125, "             0.000")
    call checkfmt("(rz,-6pf18.3)", 643.125, "             0.000")
    call checkfmt("(rz,-5pf18.3)", 643.125, "             0.006")
    call checkfmt("(rz,-4pf18.3)", 643.125, "             0.064")
    call checkfmt("(rz,-3pf18.3)", 643.125, "             0.643")
    call checkfmt("(rz,-2pf18.3)", 643.125, "             6.431")
    call checkfmt("(rz,-1pf18.3)", 643.125, "            64.312")
    call checkfmt("(rz, 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(rc,-8pf18.3)", 643.125, "             0.000")
    call checkfmt("(rc,-7pf18.3)", 643.125, "             0.000")
    call checkfmt("(rc,-6pf18.3)", 643.125, "             0.001")
    call checkfmt("(rc,-5pf18.3)", 643.125, "             0.006")
    call checkfmt("(rc,-4pf18.3)", 643.125, "             0.064")
    call checkfmt("(rc,-3pf18.3)", 643.125, "             0.643")
    call checkfmt("(rc,-2pf18.3)", 643.125, "             6.431")
    call checkfmt("(rc,-1pf18.3)", 643.125, "            64.313")
    call checkfmt("(rc, 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(rn,-8pf18.3)", 643.125, "             0.000")
    call checkfmt("(rn,-7pf18.3)", 643.125, "             0.000")
    call checkfmt("(rn,-6pf18.3)", 643.125, "             0.001")
    call checkfmt("(rn,-5pf18.3)", 643.125, "             0.006")
    call checkfmt("(rn,-4pf18.3)", 643.125, "             0.064")
    call checkfmt("(rn,-3pf18.3)", 643.125, "             0.643")
    call checkfmt("(rn,-2pf18.3)", 643.125, "             6.431")
    call checkfmt("(rn,-1pf18.3)", 643.125, "            64.312")
    call checkfmt("(rn, 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(rp,-8pf18.3)", 643.125, "             0.000")
    call checkfmt("(rp,-7pf18.3)", 643.125, "             0.000")
    call checkfmt("(rp,-6pf18.3)", 643.125, "             0.001")
    call checkfmt("(rp,-5pf18.3)", 643.125, "             0.006")
    call checkfmt("(rp,-4pf18.3)", 643.125, "             0.064")
    call checkfmt("(rp,-3pf18.3)", 643.125, "             0.643")
    call checkfmt("(rp,-2pf18.3)", 643.125, "             6.431")
    call checkfmt("(rp,-1pf18.3)", 643.125, "            64.312")
    call checkfmt("(rp, 0pf18.3)", 643.125, "           643.125")

    call checkfmt("(-8pf18.3)", -643.125, "            -0.000")
    call checkfmt("(-7pf18.3)", -643.125, "            -0.000")
    call checkfmt("(-6pf18.3)", -643.125, "            -0.001")
    call checkfmt("(-5pf18.3)", -643.125, "            -0.006")
    call checkfmt("(-4pf18.3)", -643.125, "            -0.064")
    call checkfmt("(-3pf18.3)", -643.125, "            -0.643")
    call checkfmt("(-2pf18.3)", -643.125, "            -6.431")
    call checkfmt("(-1pf18.3)", -643.125, "           -64.312")
    call checkfmt("( 0pf18.3)", -643.125, "          -643.125")

    call checkfmt("(ru,-8pf18.3)", -643.125, "            -0.000")
    call checkfmt("(ru,-7pf18.3)", -643.125, "            -0.000")
    call checkfmt("(ru,-6pf18.3)", -643.125, "            -0.000")
    call checkfmt("(ru,-5pf18.3)", -643.125, "            -0.006")
    call checkfmt("(ru,-4pf18.3)", -643.125, "            -0.064")
    call checkfmt("(ru,-3pf18.3)", -643.125, "            -0.643")
    call checkfmt("(ru,-2pf18.3)", -643.125, "            -6.431")
    call checkfmt("(ru,-1pf18.3)", -643.125, "           -64.312")
    call checkfmt("(ru, 0pf18.3)", -643.125, "          -643.125")

    call checkfmt("(rd,-8pf18.3)", -643.125, "            -0.001")
    call checkfmt("(rd,-7pf18.3)", -643.125, "            -0.001")
    call checkfmt("(rd,-6pf18.3)", -643.125, "            -0.001")
    call checkfmt("(rd,-5pf18.3)", -643.125, "            -0.007")
    call checkfmt("(rd,-4pf18.3)", -643.125, "            -0.065")
    call checkfmt("(rd,-3pf18.3)", -643.125, "            -0.644")
    call checkfmt("(rd,-2pf18.3)", -643.125, "            -6.432")
    call checkfmt("(rd,-1pf18.3)", -643.125, "           -64.313")
    call checkfmt("(rd, 0pf18.3)", -643.125, "          -643.125")

    call checkfmt("(rz,-8pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rz,-7pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rz,-6pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rz,-5pf18.3)", -643.125, "            -0.006")
    call checkfmt("(rz,-4pf18.3)", -643.125, "            -0.064")
    call checkfmt("(rz,-3pf18.3)", -643.125, "            -0.643")
    call checkfmt("(rz,-2pf18.3)", -643.125, "            -6.431")
    call checkfmt("(rz,-1pf18.3)", -643.125, "           -64.312")
    call checkfmt("(rz, 0pf18.3)", -643.125, "          -643.125")

    call checkfmt("(rc,-8pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rc,-7pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rc,-6pf18.3)", -643.125, "            -0.001")
    call checkfmt("(rc,-5pf18.3)", -643.125, "            -0.006")
    call checkfmt("(rc,-4pf18.3)", -643.125, "            -0.064")
    call checkfmt("(rc,-3pf18.3)", -643.125, "            -0.643")
    call checkfmt("(rc,-2pf18.3)", -643.125, "            -6.431")
    call checkfmt("(rc,-1pf18.3)", -643.125, "           -64.313")
    call checkfmt("(rc, 0pf18.3)", -643.125, "          -643.125")

    call checkfmt("(rn,-8pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rn,-7pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rn,-6pf18.3)", -643.125, "            -0.001")
    call checkfmt("(rn,-5pf18.3)", -643.125, "            -0.006")
    call checkfmt("(rn,-4pf18.3)", -643.125, "            -0.064")
    call checkfmt("(rn,-3pf18.3)", -643.125, "            -0.643")
    call checkfmt("(rn,-2pf18.3)", -643.125, "            -6.431")
    call checkfmt("(rn,-1pf18.3)", -643.125, "           -64.312")
    call checkfmt("(rn, 0pf18.3)", -643.125, "          -643.125")

    call checkfmt("(rp,-8pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rp,-7pf18.3)", -643.125, "            -0.000")
    call checkfmt("(rp,-6pf18.3)", -643.125, "            -0.001")
    call checkfmt("(rp,-5pf18.3)", -643.125, "            -0.006")
    call checkfmt("(rp,-4pf18.3)", -643.125, "            -0.064")
    call checkfmt("(rp,-3pf18.3)", -643.125, "            -0.643")
    call checkfmt("(rp,-2pf18.3)", -643.125, "            -6.431")
    call checkfmt("(rp,-1pf18.3)", -643.125, "           -64.312")
    call checkfmt("(rp, 0pf18.3)", -643.125, "          -643.125")

    ! print *, n_tst, n_cnt, n_skip
    if (n_cnt /= 0) call abort
    if (all(.not. l_skip)) print *, "All kinds rounded to nearest"

contains
    subroutine checkfmt(fmt, x, cmp)
        implicit none
        integer :: i
        character(len=*), intent(in) :: fmt
        real, intent(in) :: x
        character(len=*), intent(in) :: cmp
        do i=1,size(real_kinds)
          if (i == 1) then
            write(s, fmt) real(x,kind=j(1))
          else if (i == 2) then
            write(s, fmt) real(x,kind=j(2))
          else if (i == 3) then
            write(s, fmt) real(x,kind=j(3))
          else if (i == 4) then
            write(s, fmt) real(x,kind=j(4))
          end if
          n_tst = n_tst + 1
          if (s /= cmp) then
            if (l_skip(i)) then
              n_skip = n_skip + 1
            else
              print "(a,1x,a,' expected: ',1x,a)", fmt, s, cmp
              n_cnt = n_cnt + 1
            end if
          end if
        end do
        
    end subroutine
end program
! { dg-output "All kinds rounded to nearest" { xfail { i?86-*-solaris2.9* hppa*-*-hpux* } } }
