! { dg-do compile }
! { dg-options "-O3" }
!
! Tests the fix for PR85954 in which the gimplifier could not determine
! the space required for the dummy argument data types, when inlining the
! subroutines.
!
! Contributed by G.Steinmetz  <gscfq@t-online.de>
!
program p
   character(kind=1,len=:), allocatable :: z(:)
   allocate (z, source = ["xyz"])
   print *, allocated(z), size(z), len(z), z
   call s(z)
   call t(z)
contains
   subroutine s(x)
      character(kind=1,len=:), allocatable :: x(:)
      x = ['abcd']
      print *, allocated(x), size(x), len(x), x
   end
   subroutine t(x)
      character(kind=1,len=:), allocatable :: x(:)
      associate (y => x)
         y = ['abc']
      end associate
      print *, allocated(x), size(x), len(x), x
   end
end
