! { dg-do compile }
!
! Test the fix for pr95543. The variable declaration in each subroutine used to ICE
! because the substitution of a in the default initializers of b was not being done.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   call foo1
   call foo2
   call foo3
   call foo4
contains
   subroutine foo1
      type t(a, b)
         integer, kind :: a = 4
         integer, kind :: b = a + 4
      end type
      type(t()) :: z ! { dg-error "empty type specification" }
      print *, z%b
   end
   subroutine foo2
      type t(a, b)
         integer, kind :: a = 1
         integer, kind :: b = a
      end type
      type(t) :: z
      print *, z%b
   end
   subroutine foo3
      type t(a, b)
         integer, kind :: a = 1
         integer, kind :: b = a
      end type
      type(t(2)) :: z
      print *, z%b
   end
   subroutine foo4
      type t(a, b)
         integer, kind :: a = 4
         integer, kind :: b = a + 4
      end type
      type(t(b = 6)) :: z
      print *, z%b
   end
end

