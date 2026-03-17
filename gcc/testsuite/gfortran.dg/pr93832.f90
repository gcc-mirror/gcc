module m
contains
   subroutine comment0
      type t
         character :: a
         integer :: b
         integer :: c(t(1))            ! { dg-error "No initializer for component .b." }
      end type
      type(t) :: z = t('a', 2, [3])    ! { dg-error "Bad array spec of component .c." }
   end

   subroutine comment3a
      type t
         character :: a
         integer :: b
         integer :: c(t(1, "rubbish")) ! { dg-error "No initializer for component .c." }
      end type
      type(t) :: z = t('a', 2, [3])    ! { dg-error "Bad array spec of component .c." }
   end

   subroutine comment3b
      type t
         character :: a
         integer :: b
         integer :: c(t(1, "rubbish", [7])) ! { dg-error "Derived type or class expression" }
      end type
      type(t) :: z = t('a', 2, [3])    ! { dg-error "Bad array spec of component .c." }
   end

   subroutine comment9
      type t
         character :: a
         integer :: b(t(1))            ! { dg-error "No initializer for component .b." }
      end type
      type(t) :: x = t('a', 2)
   end
end module
