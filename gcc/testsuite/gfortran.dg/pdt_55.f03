! { dg-do run }
!
! Test fix for PRs 102240, 102686 and 93175.
!
! PR102240
! Contributed by Roland Wirth  <roland_wirth@web.de>
!
MODULE m1
   IMPLICIT NONE
   private
   public r
   INTEGER :: n0, n       ! Symbols that confused the parameter substitution.
   type t0(m0,n0)
      INTEGER, kind :: m0
      INTEGER, LEN :: n0
      INTEGER(kind=m0) :: a0(n0*2)
      end type t0

   TYPE t(m,n)
      INTEGER, kind :: m
      INTEGER, LEN :: n
      INTEGER(kind=m) :: a(n/8:(n/2 + 4))
      type(t0(m,n)) :: p  ! During testing, getting this to work fixed PR93175.
   END TYPE t
contains
   subroutine r
      type (t(kind(1_8), 8)) :: x
      x%a = [1,2,3,4,5,6,7,8]
      if (kind (x%a) /= kind(1_8)) stop 1
      if (sum (x%a) /= 36_8) stop 2
      if (size(x%p%a0) /= 16) stop 3
   end
END

! PR102686
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m2
   implicit none
   private
   public s
contains
   pure integer function n()    ! Confused the parameter substitution.
      n = 1
   end
   subroutine s
      type t(n)
         integer, len :: n = 2
         character(len=n) :: c  ! ICE because function n() referenced rather than parameter.
      end type
      type (t(4)) :: c_type, c_type2
      c_type = t(4)("abcd")
      if (len (c_type%c) /= 4) stop 4
      if (c_type%c /= "abcd") stop 5
      c_type2%c = "efgh"
      if (len (c_type2%c) /= 4) stop 6
      if (c_type2%c /= "efgh") stop 7
   end
end

! PR93175
! Contributed by Rich Townsend  <townsend@astro.wisc.edu>
!
module m3
   private
   public u
   type :: matrix (k,n)
      integer, kind :: k
      integer, len  :: n
      real(k)       :: a(n,n)
   end type matrix

   type :: problem(n)
      integer, len               :: n
      type(matrix(kind(0.D0),n)) :: m
   end type problem

contains
   subroutine u
     implicit none
     type(problem(2)) :: p

     p%m%a = 1.
     if (p%n /= 2) stop 8
     if (p%m%n /= 2) stop 9
     if (int (sum (p%m%a)) /= 4) stop 10
  end subroutine
end module m3

   use m1
   use m2
   use m3
   call r
   call s
   call u
end
