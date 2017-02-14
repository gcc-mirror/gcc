! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! Check that error message is presented as long as polymorphic coarrays are
! not implemented.

module maccscal
   type t
      real, allocatable :: a
   end type
contains
   subroutine s(x) ! { dg-error "Sorry, allocatable/pointer components in polymorphic \\(CLASS\\)" }
      class(t) :: x[*]
      allocate (x%a)
   end
end
module mptrscal
   type t
      real, pointer :: a
   end type
contains
   subroutine s(x) ! { dg-error "Sorry, allocatable/pointer components in polymorphic \\(CLASS\\)" }
      class(t) :: x[*]
      allocate (x%a)
   end
end
module mallarr
   type t
      real, allocatable :: a(:)
   end type
contains
   subroutine s(x) ! { dg-error "Sorry, allocatable/pointer components in polymorphic \\(CLASS\\)" }
      class(t) :: x[*]
      allocate (x%a(2))
   end
end
module mptrarr
   type t
      real, pointer :: a(:)
   end type
contains
   subroutine s(x) ! { dg-error "Sorry, allocatable/pointer components in polymorphic \\(CLASS\\)" }
      class(t) :: x[*]
      allocate (x%a(2))
   end
end
