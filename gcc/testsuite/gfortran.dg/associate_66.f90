! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Tests unlimited polymorphic function selectors in ASSOCIATE.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de> in
! https://gcc.gnu.org/pipermail/fortran/2024-January/060098.html
!
program p
   implicit none
!             scalar             array
   associate (var1 => foo1(),    var2 => foo2())
    call prt (var1);   call prt (var2)
   end associate
contains
! Scalar value
   function foo1() result(res)
     class(*), allocatable :: res
     res = 42.0
   end function foo1
! Array value
   function foo2() result(res)
     class(*), allocatable :: res(:)
     res = [42, 84]
   end function foo2
! Test the associate-name value
   subroutine prt (x)
     class(*), intent(in) :: x(..)
     logical :: ok = .false.
     select rank(x)
       rank (0)
         select type (x)
           type is (real)
           if (int(x*10) .eq. 420) ok = .true.
         end select
       rank (1)
         select type (x)
           type is (integer)
           if (all (x .eq. [42, 84])) ok = .true.
         end select
     end select
     if (.not.ok) stop 1
   end subroutine prt
end
! { dg-final { scan-tree-dump-times "__builtin_free" 2 "original" } }
