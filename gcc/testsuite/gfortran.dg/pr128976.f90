! { dg-do compile }
!
! PR 128976

module m
  implicit none
  type :: t1_t
    integer :: i = 0
  end type t1_t
contains
  subroutine s1 (core)
    class(t1_t), intent(in), pointer :: core
    select type (core)
    class is (t1_t)
       call s2 (core)
       core%i = 1
    end select
  end subroutine s1
  subroutine s3 (core)
    class(*), intent(in), pointer :: core
    select type (core)
    class is (t1_t)
       call s2 (core)
    type is (integer)
       core = 1
    end select
  end subroutine s3
  subroutine s4 (core)
    class(t1_t), intent(in), pointer :: core(:)
    call s2 (core(1))
    select type (core)
    class is (t1_t)
       core%i = 1
       call s2 (core(2))
    end select
  end subroutine s4
  subroutine s5 (core)
    class(t1_t), intent(in) :: core
    select type (core)
    class is (t1_t)
       call s2 (core)   ! { dg-error "INTENT\\(IN\\)" }
       core%i = 1       ! { dg-error "INTENT\\(IN\\)" }
    end select
  end subroutine s5
  subroutine s2 (core)
    class(t1_t), intent(inout) :: core
  end subroutine s2
end module m
