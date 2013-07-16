! { dg-do compile }
!
! PR 56814: [4.8/4.9 Regression] Bogus Interface mismatch in dummy procedure
!
! Contributed by Marco Restelli <mrestelli@gmail.com>

module m1
  abstract interface
    pure function i_f(x) result(d)
    real, intent(in) :: x(:,:)
    real :: d(size(x,1),size(x,2))
    end function
  end interface

  procedure(i_f), pointer :: f => null()
end module

module m2
contains
  pure subroutine ns_dirdata(fun)
    interface
    pure function fun(x) result(d)
      real, intent(in) :: x(:,:)
      real :: d(size(x,1),size(x,2))
    end function
    end interface
  end subroutine
end module

program p
 use m1
 use m2
  call ns_dirdata(f)
end

! { dg-final { cleanup-modules "m1 m2" } }
