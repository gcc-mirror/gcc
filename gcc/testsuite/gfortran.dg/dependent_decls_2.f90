! { dg-do run }
!
! Fix for PR59104 in which the dependence on the old style function result
! was not taken into account in the ordering of auto array allocation and
! characters with dependent lengths.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module m
   implicit none
   integer, parameter :: dp = kind([double precision::])
   contains
      function f(x)
         integer, intent(in) :: x
         real(dp) f(x/2)
         real(dp) g(x/2)
         integer y(size (f)+1)              ! This was the original problem
         integer z(size (f) + size (y))     ! Found in development of the fix
         integer w(size (f) + size (y) + x) ! Check dummy is OK
         integer :: l1(size(y))
         integer :: l2(size(z))
         integer :: l3(size(w))
         f = 10.0
         y = 1                              ! Stop -Wall from complaining
         z = 1; g = 1; w = 1; l1 = 1; l2 = 1; l3 = 1
         if (size (f) .ne. 1) stop 1
         if (size (g) .ne. 1) stop 2
         if (size (y) .ne. 2) stop 3
         if (size (z) .ne. 3) stop 4
         if (size (w) .ne. 5) stop 5
         if (size (l1) .ne. 2) stop 6       ! Check indirect dependencies
         if (size (l2) .ne. 3) stop 7
         if (size (l3) .ne. 5) stop 8

      end function f
      function e(x) result(f)
         integer, intent(in) :: x
         real(dp) f(x/2)
         real(dp) g(x/2)
         integer y(size (f)+1)
         integer z(size (f) + size (y))     ! As was this.
         integer w(size (f) + size (y) + x)
         integer :: l1(size(y))
         integer :: l2(size(z))
         integer :: l3(size(w))
         f = 10.0
         y = 1; z = 1; g = 1; w = 1; l1 = 1; l2 = 1; l3 = 1
         if (size (f) .ne. 2) stop 9
         if (size (g) .ne. 2) stop 10
         if (size (y) .ne. 3) stop 11
         if (size (z) .ne. 5) stop 12
         if (size (w) .ne. 9) stop 13
         if (size (l1) .ne. 3) stop 14      ! Check indirect dependencies
         if (size (l2) .ne. 5) stop 15
         if (size (l3) .ne. 9) stop 16
      end function
      function d(x)  ! After fixes to arrays, what was needed was known!
        integer, intent(in) :: x
        character(len = x/2) :: d
        character(len = len (d)) :: line
        character(len = len (d) + len (line)) :: line2
        character(len = len (d) + len (line) + x) :: line3
! Commented out lines give implicit type warnings with gfortran and nagfor
!        character(len = len (d)) :: line4 (len (line3))
        character(len = len (line3)) :: line4 (len (line3))
!        character(len = size(len4, 1)) :: line5
        line = repeat ("a", len (d))
        line2 = repeat ("b", x)
        line3 = repeat ("c", len (line3))
        if (len (line2) .ne. x) stop 17
        if (line3 .ne. "cccccccc") stop 18
        d = line
        line4 = line3
        if (size (line4) .ne. 8) stop 19
        if (any (line4 .ne. "cccccccc")) stop 20
      end
end module m

program p
   use m
   implicit none
   real(dp) y

   y = sum (f (2))
   if (int (y) .ne. 10) stop 21
   y = sum (e (4))
   if (int (y) .ne. 20) stop 22
   if (d (4) .ne. "aa") stop 23
end program p
