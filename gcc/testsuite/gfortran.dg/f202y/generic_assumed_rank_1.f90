! { dg-do run }
! { dg-options "-fcheck=bounds" }
!
! Test Reinhold Bader's F202y proposal (J3 DIN4) "Generic processing of assumed
! rank objects". The present gfortran implementation includes pointer assignment
! and ASSOCIATE, with rank remapping of the var or associate-name, and RESHAPE.
! J3 document 24-136r1.txt, by Malcolm Cohen, considers further possibilities.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
   real :: x(2,2,2)
   real, parameter :: xp(*) = [1,2,3,4,5,6,7,8]
   x = reshape (xp, [2,2,2])
   call my_sub (x)
   if (any (reshape (x, [8]) .ne. xp(8:1:-1))) stop 1
   call my_assumed_size_target (x)
contains
   subroutine my_sub (arg)
     real, target, contiguous :: arg(..)
     real, allocatable :: y(:)
     real, pointer :: argp(:,:)
     integer :: i

     if (size (arg) .lt. 0) return

     if (size (arg) .ne. 8) stop 10

! Check reshape
     y = reshape (arg, [size (arg)])
     if (any (y .ne. xp)) stop 20

! Check pointer assignment
     argp(1:2,1: size(arg)/2) => arg
     if (size (argp) .ne. size (x)) stop 30
     if (any ((argp) .ne. reshape (x, [2, size (x)/2]))) stop 31

! Check ASSOCIATE
     i = size (arg)
     associate (a(1:2,1:i/2) => arg)
        if (any (a .ne. argp)) stop 40
     end associate

     associate (a(1:size(arg)) => arg)
        if (any (a .ne. xp)) stop 41
        a = a(8:1:-1)
     end associate
   end

   subroutine my_assumed_size_target (arg)
     real :: arg(2, 2, *)
     call my_sub (arg)
   end
end
! { dg-output "Fortran runtime warning: Assumed rank object arg is associated with an assumed size object" }
