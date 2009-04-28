! { dg-do compile }
!
! PR 39930: Bogus error: ambiguous reference
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module a1
contains
  subroutine myRoutine
  end subroutine
end module 

module a2
contains
  subroutine myRoutine
  end subroutine
end module 

module b
contains

  subroutine otherRoutine
    use a1
    use a2
  end subroutine

  subroutine myRoutine
  end subroutine myRoutine      ! this is not ambiguous !

end module

! { dg-final { cleanup-modules "a1 a2 b" } }

