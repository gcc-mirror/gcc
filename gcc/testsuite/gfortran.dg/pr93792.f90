! { dg-do compile }
! Original test case by Gernhard Steinmetz.

module m
   type t(n)
      integer, len :: n = z'1'
   end type
end
program p
   use m
   type(t(:)), allocatable :: z
end

! { dg-error "Parameterized type 't' does not have a component"  " " { target *-*-* } 5 }
! { dg-error "BOZ literal constant at .1. cannot appear"  " " { target *-*-* } 6 }
! { dg-error "Cannot open module file"  " " { target *-*-* } 10 }
! { dg-prune-output "compilation terminated" }
