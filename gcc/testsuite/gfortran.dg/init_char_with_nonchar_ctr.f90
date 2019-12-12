! { dg-do compile }

! Initialization of character by non-character constructor

subroutine s1
   type t
      integer :: n = 0
   end type
   type t2
      character :: c = t() ! { dg-error "Cannot convert TYPE\\(t\\) to CHARACTER\\(1\\)" }
   end type
end subroutine

subroutine s2
   type t
   end type
   type t2
      character :: c(1) = [t()] ! { dg-error "Cannot convert TYPE\\(t\\) to CHARACTER\\(1\\)" }
   end type
end subroutine

subroutine s3
   type t
      integer :: a = 1
      character :: c = t() ! { dg-error "Cannot convert TYPE\\(t\\) to CHARACTER\\(1\\)" }
   end type
end subroutine

subroutine s4
   type t
      integer, allocatable :: a
      character :: c = t() ! { dg-error "Cannot convert TYPE\\(t\\) to CHARACTER\\(1\\)" }
   end type
end subroutine
