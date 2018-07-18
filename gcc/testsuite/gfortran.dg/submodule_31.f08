! { dg-do run }
!
! Test the fix for PR82814 in which an ICE occurred for the submodule allocation.
!
! Contributed by "Werner Blokbuster"  <werner.blokbuster@gmail.com>
!
module u

    implicit none

    interface unique
        module function uniq_char(input) result(uniq)
            character(*), intent(in) :: input(:)
            character(size(input)), allocatable :: uniq(:)
        end function uniq_char
    end interface unique

contains

    module function uniq2(input) result(uniq)
        character(*), intent(in) :: input(:)
        character(size(input)), allocatable :: uniq(:)
            allocate(uniq(1))
            uniq = 'A'
    end function uniq2

end module u


submodule (u) z

    implicit none

contains

    module function uniq_char(input) result(uniq)
        character(*), intent(in) :: input(:)
        character(size(input)), allocatable :: uniq(:)
            allocate(uniq(1)) ! This used to ICE
            uniq = 'A'
    end function uniq_char

end submodule z


program test_uniq
    use u
    implicit none
    character(1), dimension(4) :: chr = ['1','2','1','2']

    write(*,*) unique(chr)
    write(*,*) uniq2(chr)

end program test_uniq
