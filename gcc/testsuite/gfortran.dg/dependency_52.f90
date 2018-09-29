! { dg-do run }
!
! Test the fix for PR65667, in which the dependency was missed and
! the string length of 'text' was decremented twice. The rhs string
! length is now fixed after the function call so that the dependency
! on the length of 'text' is removed for later evaluations.
!
!Contributed by John  <jwmwalrus@gmail.com>
!
module mod1
    implicit none
contains
    subroutine getKeyword(string, keyword, rest)
        character(:), allocatable, intent(IN) :: string
        character(:), allocatable, intent(OUT) :: keyword, rest
        integer :: idx
        character(:), allocatable :: text

        keyword = ''
        rest = ''
        text = string
        text = ADJUSTL(text(2:))    ! Note dependency.
        idx = INDEX(text, ' ')

        if (idx == 0) then
            keyword = TRIM(text)
        else
            keyword = text(:idx-1)
            rest = TRIM(ADJUSTL(text(idx+1:)))
        endif
    end subroutine
end module mod1

    use mod1
    implicit none

    character(:), allocatable :: line, keyword, rest

    line = '@HERE    IT IS'

    call getKeyword(line, keyword, rest)

    if (keyword .ne. 'HERE') stop 1
    if (rest .ne. 'IT IS') stop 2
end
