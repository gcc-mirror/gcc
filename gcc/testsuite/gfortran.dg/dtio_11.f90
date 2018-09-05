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
      type(unknown), pointer :: next ! { dg-error "has not been declared" }
   contains
      procedure :: s  ! { dg-error "Non-polymorphic passed-object" }
      generic :: write(formatted) => s
   end type
contains
   subroutine s(x)  ! { dg-error "Too few dummy arguments" }
   end
end

! PR77533 comment #1 - gave error 'KIND = 0'
module m3
   type t
   contains
      procedure :: s ! { dg-error "Non-polymorphic passed-object" }
      generic :: write(formatted) => s
   end type
contains
   subroutine s(x) ! { dg-error "Too few dummy arguments" }
      class(t), intent(in) : x ! { dg-error "Invalid character in name" }
   end
end

! PR77534
module m4
   type t
   end type
   interface read(unformatted)
      module procedure s
   end interface
contains
   subroutine s(dtv) ! { dg-error "Too few dummy arguments" }
      type(t), intent(inout) :: dtv
   end
end
