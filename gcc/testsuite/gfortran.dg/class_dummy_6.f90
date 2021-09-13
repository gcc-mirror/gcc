! { dg-do run }
!
! Test the fix for PR99819 - explicit shape class arrays in different
! procedures caused an ICE.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer :: i
   end type
   class(t), allocatable :: dum1(:), dum2(:), dum3(:,:)

   allocate (t :: dum1(3), dum2(10), dum3(2,5))
   dum2%i = [1,2,3,4,5,6,7,8,9,10]
   dum3%i = reshape ([1,2,3,4,5,6,7,8,9,10],[2,5])

! Somewhat elaborated versions of the PR procedures.
   if (f (dum1, dum2, dum3) .ne. 10) stop 1
   if (g (dum1) .ne. 3) stop 2

! Test the original versions of the procedures.
   if (f_original (dum1, dum2) .ne. 3) stop 3
   if (g_original (dum2) .ne. 10) stop 4

contains
   integer function f(x, y, z)
      class(t) :: x(:)
      class(t) :: y(size( x))
      class(t) :: z(2,*)
      if (size (y) .ne. 3) stop 5
      if (size (z) .ne. 0) stop 6
      select type (y)
        type is (t)
          f = 1
          if (any (y%i .ne. [1,2,3])) stop 7
        class default
          f = 0
      end select
      select type (z)
        type is (t)
          f = f*10
          if (any (z(1,1:4)%i .ne. [1,3,5,7])) stop 8
        class default
          f = 0
      end select
   end
   integer function g(z)
      class(t) :: z(:)
      type(t) :: u(size(z))
      g = size (u)
   end

   integer function f_original(x, y)
      class(t) :: x(:)
      class(*) :: y(size (x))
      f_original = size (y)
   end

   integer function g_original(z)
      class(*) :: z(:)
      type(t) :: u(size(z))
      g_original = size (u)
   end
end
