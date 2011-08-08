! { dg-do compile }
! reduced testcase from PR 17535
module FOO
  interface BAR

    subroutine BAR1(X)
      integer :: X
    end subroutine

    subroutine BAR2(X)
      real :: X
    end subroutine

  end interface
end module

subroutine BAZ(X)
  use FOO
end subroutine

! { dg-final { cleanup-modules "foo" } }
