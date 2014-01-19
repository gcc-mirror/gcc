! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/34547
! PR fortran/50375

subroutine test_PR50375_3 ()
  interface gen3
    subroutine s31 (pi)
      integer, pointer :: pi
    end subroutine
    subroutine s32 (pr)
      real, allocatable :: pr(:)
    end subroutine
  end interface
  call gen3 (null ()) ! OK
end subroutine test_PR50375_3

subroutine test_PR50375_2 ()
  interface gen2
    subroutine s21 (pi)
      integer, pointer :: pi
    end subroutine
    subroutine s22 (pr)
      real, optional :: pr
    end subroutine
  end interface
  call gen2 (null ()) ! { dg-error "MOLD= required in NULL|There is no specific subroutine" }
end subroutine test_PR50375_2

subroutine test_PR34547_3 ()
  integer, allocatable :: i(:)
  print *, NULL(i)    ! { dg-error "Invalid context for NULL" }
end subroutine test_PR34547_3
