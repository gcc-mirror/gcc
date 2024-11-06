! { dg-do run }
! { dg-options "-std=f202y -Wsurprising" }
!
! Test Reinhold Bader's F202y proposal "Generic processing of assumed rank objects".
! Tests class assumed rank objects.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
   type :: t1
     integer :: i
   end type
   type, extends(t1) :: t2
     integer :: j
   end type

   class(t1), allocatable :: x(:,:)
   type(t2), parameter :: xp(*) = [t2(t1(1),2),t2(t1(3),4),t2(t1(5),6),t2(t1(7),8)]
   x = reshape (xp, [2,2])
   call my_sub1 (x)
   if (any (x(2:1:-1,2:1:-1)%i .ne. reshape (xp%i, [2,2]))) stop 1
   call my_sub2 (x)
   if (any (x(2:1:-1,2:1:-1)%i .ne. reshape (xp%i, [2,2]))) stop 2
   deallocate (x)
contains
   subroutine my_sub1 (class_arg)
      class(t1), contiguous, target :: class_arg(..)
      class(t1), pointer :: cp(:)
      integer :: cp_sz
      integer :: lb(1)
      integer :: ub(1)
      integer :: slb = 2

      cp_sz = size (class_arg)
      cp(slb:slb+cp_sz-1) => class_arg
      if (any (cp%i .ne. xp%i)) stop 3
      if (size (cp) .ne. cp_sz) stop 4
      if (ubound (cp, 1) .ne. slb+cp_sz-1) stop 5

      associate (ca(slb:slb+cp_sz-1) => class_arg)
         lb = lbound (ca)
         ub = ubound (ca)
         if (size (ca) .ne. cp_sz) stop 6
         if (ubound (ca, 1) .ne. slb+cp_sz-1) stop 7
         select type (ca)
            type is (t2)
               ca = ca(ub(1):lb(1):-1)
            class default
         end select
      end associate
   end

   subroutine my_sub2 (class_arg)
      class(*), contiguous, target :: class_arg(..)
      class(*), pointer :: cp(:, :)
      integer :: cp_sz
      cp_sz = size (class_arg)
      cp(1:cp_sz/2, 1:cp_sz/2) => class_arg
      call check (cp, cp_sz)
      associate (ca(2:3,1:2) => class_arg)
         select type (ca)
            type is (t2)
               ca = ca(3:2:-1,2:1:-1)
            class default
         end select
      end associate
   end

   subroutine check (arg, sz)
      class(*), intent(inOUT) :: arg(:, :)
      integer :: sz
      integer :: lb(2)
      integer :: ub(2)
      lb = lbound(arg)
      ub = ubound(arg)
      select type (s => arg)
         type is (t2)
            s = s(ub(1):lb(1):-1,ub(2):lb(1):-1)
            if (any (reshape (s(lb(1):ub(1),lb(2):ub(2))%j, [sz]) &
                .ne. xp%j)) stop 8

         class default
            stop 9
      end select
   end
end
