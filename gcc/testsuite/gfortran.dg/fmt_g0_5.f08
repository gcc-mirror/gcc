! { dg-do run }
! { dg-add-options ieee }
! PR48589 Invalid G0/G0.d editing for NaN/infinity
! Test case by Thomas Henlich
program test_g0_special

    call check_all("(g10.3)", "(f10.3)")
    call check_all("(g10.3e3)", "(f10.3)")
    call check_all("(spg10.3)", "(spf10.3)")
    call check_all("(spg10.3e3)", "(spf10.3)")
    !print *, "-----------------------------------"
    call check_all("(g0)", "(f0.0)")
    call check_all("(g0.15)", "(f0.0)")
    call check_all("(spg0)", "(spf0.0)")
    call check_all("(spg0.15)", "(spf0.0)")
contains
    subroutine check_all(fmt1, fmt2)
        character(len=*), intent(in) :: fmt1, fmt2
        real(8) :: one = 1.0D0, zero = 0.0D0, nan, pinf, minf

        nan = zero / zero
        pinf = one / zero
        minf = -one / zero
        call check_equal(fmt1, fmt2, nan)
        call check_equal(fmt1, fmt2, pinf)
        call check_equal(fmt1, fmt2, minf)
    end subroutine check_all
    subroutine check_equal(fmt1, fmt2, r)
        real(8), intent(in) :: r
        character(len=*), intent(in) :: fmt1, fmt2
        character(len=80) :: s1, s2
        
        write(s1, fmt1) r
        write(s2, fmt2) r
        if (s1 /= s2) STOP 1
        !if (s1 /= s2) print "(6a)", trim(fmt1), ": '", trim(s1), "' /= '", trim(s2), "'"
	!print "(6a)", trim(fmt1), ": '", trim(s1), "' /= '", trim(s2), "'"
    end subroutine check_equal
end program test_g0_special
