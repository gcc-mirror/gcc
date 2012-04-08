! { dg-do compile }
! { dg-options "-O2" }
!
! PR fortran/52751 (top, "module mod")
! PR fortran/40973 (bottom, "module m"
!
! Ensure that (only) those module variables and procedures which are PRIVATE
! and have no C-binding label are optimized away.
!
      module mod
        integer :: aa
        integer, private :: iii
        integer, private, bind(C) :: jj             ! { dg-warning "PRIVATE but has been given the binding label" }
        integer, private, bind(C,name='lll') :: kk  ! { dg-warning "PRIVATE but has been given the binding label" }
        integer, private, bind(C,name='') :: mmmm
        integer, bind(C) :: nnn
        integer, bind(C,name='oo') :: pp
        integer, bind(C,name='') :: qq
      end module mod

      ! { dg-final { scan-assembler "__mod_MOD_aa" } }
      ! { dg-final { scan-assembler-not "iii" } }
      ! { dg-final { scan-assembler "jj" } }
      ! { dg-final { scan-assembler "lll" } }
      ! { dg-final { scan-assembler-not "kk" } }
      ! { dg-final { scan-assembler-not "mmmm" } }
      ! { dg-final { scan-assembler "nnn" } }
      ! { dg-final { scan-assembler "oo" } }
      ! { dg-final { scan-assembler "__mod_MOD_qq" } }

MODULE M
  PRIVATE :: two, three, four, six
  PUBLIC :: one, seven, eight, ten
CONTAINS
  SUBROUTINE one(a)
    integer :: a
    a = two()
  END SUBROUTINE one
  integer FUNCTION two()
     two = 42
  END FUNCTION two
  integer FUNCTION three() bind(C) ! { dg-warning "PRIVATE but has been given the binding label" }
     three = 43
  END FUNCTION three
  integer FUNCTION four() bind(C, name='five') ! { dg-warning "PRIVATE but has been given the binding label" }
     four = 44
  END FUNCTION four
  integer FUNCTION six() bind(C, name='')
     six = 46
  END FUNCTION six
  integer FUNCTION seven() bind(C)
     seven = 46
  END FUNCTION seven
  integer FUNCTION eight() bind(C, name='nine')
     eight = 48
  END FUNCTION eight
  integer FUNCTION ten() bind(C, name='')
     ten = 48
  END FUNCTION ten
END MODULE

! { dg-final { scan-assembler "__m_MOD_one" } }
! { dg-final { scan-assembler-not "two" } }
! { dg-final { scan-assembler "three" } }
! { dg-final { scan-assembler-not "four" } }
! { dg-final { scan-assembler "five" } }
! { dg-final { scan-assembler-not "six" } }
! { dg-final { scan-assembler "seven" } }
! { dg-final { scan-assembler "nine" } }
! { dg-final { scan-assembler "__m_MOD_ten" } }
