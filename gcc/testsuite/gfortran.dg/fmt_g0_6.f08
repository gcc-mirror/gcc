! { dg-do run }
! { dg-options "-ffloat-store" }
! PR48602 Invalid F conversion of G descriptor for values close to powers of 10
! Test case provided by Thomas Henlich
program test_g0fr
    use iso_fortran_env
    implicit none
    integer, parameter :: RT = REAL64
    
    call check_all(0.0_RT, 15, 2, 0)
    call check_all(0.991_RT, 15, 2, 0)
    call check_all(0.995_RT, 15, 2, 0)
    call check_all(0.996_RT, 15, 2, 0)
    call check_all(0.999_RT, 15, 2, 0)
contains
    subroutine check_all(val, w, d, e)
        real(kind=RT), intent(in) :: val
        integer, intent(in) :: w
        integer, intent(in) :: d
        integer, intent(in) :: e

        call check_f_fmt(val, 'C', w, d, e)
        call check_f_fmt(val, 'U', w, d, e)
        call check_f_fmt(val, 'D', w, d, e)
    end subroutine check_all
    
    subroutine check_f_fmt(val, roundmode, w, d, e)
        real(kind=RT), intent(in) :: val
        character, intent(in) :: roundmode
        integer, intent(in) :: w
        integer, intent(in) :: d
        integer, intent(in) :: e
        character(len=80) :: fmt_f, fmt_g
        character(len=80) :: s_f, s_g
        real(kind=RT) :: mag, lower, upper
        real(kind=RT) :: r
        integer :: n, dec

        mag = abs(val)
        if (e == 0) then
            n = 4
        else
            n = e + 2
        end if
        select case (roundmode)
            case('U')
                r = 1.0_RT
            case('D')
                r = 0.0_RT
            case('C')
                r = 0.5_RT
        end select

        if (mag == 0) then
            write(fmt_f, "('R', a, ',F', i0, '.', i0, ',', i0, 'X')") roundmode, w - n, d - 1, n
        else
            do dec = d, 0, -1
                lower = 10.0_RT ** (d - 1 - dec) - r * 10.0_RT ** (- dec - 1)
                upper = 10.0_RT ** (d - dec) - r * 10.0_RT ** (- dec)
                if (lower <= mag .and. mag < upper) then
                    write(fmt_f, "('R', a, ',F', i0, '.', i0, ',', i0, 'X')") roundmode, w - n, dec, n
                    exit
                end if
            end do
        end if
        if (len_trim(fmt_f) == 0) then
            ! e editing
            return
        end if
        if (e == 0) then
            write(fmt_g, "('R', a, ',G', i0, '.', i0)") roundmode, w, d
        else
            write(fmt_g, "('R', a, ',G', i0, '.', i0, 'e', i0)") roundmode, w, d, e
        end if
        write(s_g, "('''', " // trim(fmt_g) // ",'''')") val
        write(s_f, "('''', " // trim(fmt_f) // ",'''')") val
        if (s_g /= s_f) call abort
        !if (s_g /= s_f) then
            !print "(a,g0,a,g0)", "lower=", lower, " upper=", upper
           ! print "(a, ' /= ', a, ' ', a, '/', a, ':', g0)", trim(s_g), trim(s_f), trim(fmt_g), trim(fmt_f), val
        !end if
    end subroutine check_f_fmt
end program test_g0fr
