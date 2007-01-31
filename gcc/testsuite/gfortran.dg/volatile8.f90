! Check for compatibily of actual arguments
! with dummy arguments marked as volatile
! 
! Contributed by Steven Correll.
!
! PR fortran/30520

! { dg-do compile }

   subroutine s8()
    implicit none
    interface
      subroutine sub8(dummy8)
        integer, volatile, dimension(3) :: dummy8
      end subroutine sub8
      subroutine sub8a(dummy8a)
        integer, volatile, dimension(:) :: dummy8a
      end subroutine sub8a
    end interface
    integer, dimension(8) :: a
    call sub8 (a(1:5:2)) ! { dg-error "Array-section actual argument" }
    call sub8a(a(1:5:2))
  end subroutine s8 

  subroutine s9(s9dummy)
    implicit none
    integer, dimension(:) :: s9dummy
    interface
      subroutine sub9(dummy9)
        integer, volatile, dimension(3) :: dummy9
      end subroutine sub9
      subroutine sub9a(dummy9a)
        integer, volatile, dimension(:) :: dummy9a
      end subroutine sub9a
    end interface
    integer, dimension(9) :: a
    call sub9 (s9dummy) ! { dg-error "Assumed-shape actual argument" }
    call sub9a(s9dummy)
  end subroutine s9 

  subroutine s10()
    implicit none
    interface
      subroutine sub10(dummy10)
        integer, volatile, dimension(3) :: dummy10
      end subroutine sub10
      subroutine sub10a(dummy10a)
        integer, volatile, dimension(:) :: dummy10a
      end subroutine sub10a
      subroutine sub10b(dummy10b)
        integer, volatile, dimension(:), pointer :: dummy10b
      end subroutine sub10b
    end interface
    integer, dimension(:), pointer :: a
    call sub10 (a) ! { dg-error "Pointer-array actual argument" }
    call sub10a(a)
    call sub10b(a)
  end subroutine s10 
