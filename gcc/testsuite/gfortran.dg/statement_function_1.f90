! { dg-do compile }
! PR fortran/84276
      subroutine stepns(hh, h, s, w)
      real, intent(inout) :: h, hh, s
      real, intent(out) :: w
      real :: qofs
      integer i
      qofs(s) = s
      w = qofs(hh + h)
      i = 42
      w = qofs(i)       ! { dg-error "Type mismatch in argument" }
      end subroutine stepns

      subroutine step(hh, h, s, w)
      real, intent(inout) :: h, hh, s
      real, intent(out) :: w
      real :: qofs
      integer i
      qofs(s, i) = i * s
      i = 42
      w = qofs(hh, i)
      w = qofs(i = i, s = hh) ! { dg-error "invalid in a statement function" }
      end subroutine step
! { dg-prune-output " Obsolescent feature" }
