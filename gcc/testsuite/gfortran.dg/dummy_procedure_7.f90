! { dg-do run }
!
! PR fortran/52022
!

module check
  integer, save :: icheck = 0
end module check

module t
implicit none
      contains
subroutine  sol(cost)
   use check
   interface 
        function cost(p) result(y) 
                double precision,dimension(:) :: p
                double precision,dimension(:),allocatable :: y
        end function cost
   end interface

   if (any (cost([1d0,2d0]) /= [2.d0, 4.d0])) STOP 1
   icheck = icheck + 1
end subroutine

end module t

module tt
   procedure(cost1),pointer :: pcost
contains
  subroutine init()
        pcost=>cost1
  end subroutine

  function cost1(x) result(y)
        double precision,dimension(:) :: x
        double precision,dimension(:),allocatable :: y
        allocate(y(2))
        y=2d0*x
  end function cost1



  function cost(x) result(y)
        double precision,dimension(:) :: x
        double precision,dimension(:),allocatable :: y
        allocate(y(2))
        y=pcost(x)
  end function cost
end module

program test
        use tt
        use t
        use check
        implicit none

        call init()
        if (any (cost([3.d0,7.d0]) /= [6.d0, 14.d0])) STOP 2
        if (icheck /= 0) STOP 3
        call sol(cost)
        if (icheck /= 1) STOP 4
end program test
