! { dg-do run }
!
! Contributed by Melven Roehrig-Zoellner  <Melven.Roehrig-Zoellner@DLR.de>
! PR fortran/66035

program test_pr66035
  type t
  end type t
  type w
    class(t), allocatable :: c
  end type w

  type(t) :: o

  call test(o)
contains
  subroutine test(o)
    class(t), intent(inout) :: o
    type(w), dimension(:), allocatable :: list

    select type (o)
      class is (t)
        list = [w(o)] ! This caused an ICE
      class default
        STOP 1
    end select
  end subroutine
end program
