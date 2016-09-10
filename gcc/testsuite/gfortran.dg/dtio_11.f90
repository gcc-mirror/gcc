! { dg-do compile }
!
! Test fixes for PRs77532-4.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
! PR77532 - used to ICE
module m1
   type t
   end type
   interface read(unformatted)
   end interface
end

! PR77533 - used to ICE after error
module m2
   type t
      type(unknown), pointer :: next ! { dg-error "is a type that has not been declared" }
   contains
      procedure :: s
      generic :: write(formatted) => s
   end type
contains
   subroutine s(x)
   end
end

! PR77533 comment #1 - gave warning that
module m3
   type t
   contains
      procedure :: s ! { dg-error "Non-polymorphic passed-object" }
      generic :: write(formatted) => s
   end type
contains
   subroutine s(x) ! { dg-error "must be of type CLASS" }
      class(t), intent(in) : x ! { dg-error "Invalid character in name" }
   end
end
