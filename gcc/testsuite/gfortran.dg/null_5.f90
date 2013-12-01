! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/34547
! PR fortran/50375

subroutine test_PR50375_1 ()
  ! Contributed by Vittorio Zecca
  interface gen1
    subroutine s11 (pi)
      integer, pointer :: pi
    end subroutine
    subroutine s12 (pr)
      real, pointer :: pr
    end subroutine
  end interface
  call gen1 (null ()) ! { dg-error "MOLD= required in NULL|There is no specific subroutine" }
end subroutine test_PR50375_1

subroutine test_PR50375_2 ()
  interface gen2
    subroutine s21 (pi)
      integer, pointer :: pi
    end subroutine
    subroutine s22 (pr)
      real, optional :: pr
    end subroutine
  end interface
  call gen2 (null ()) ! OK in F95/F2003 (but not in F2008)
end subroutine test_PR50375_2

subroutine test_PR34547_1 ()
  call proc (null ()) ! { dg-error "MOLD argument to NULL required" }
end subroutine test_PR34547_1

subroutine test_PR34547_2 ()
  print *, null () ! { dg-error "Invalid context" }
end subroutine test_PR34547_2

subroutine test_PR34547_3 ()
  integer, allocatable :: i(:)
  print *, NULL(i) ! { dg-error "Fortran 2003: NULL intrinsic with allocatable MOLD" }
end subroutine test_PR34547_3
