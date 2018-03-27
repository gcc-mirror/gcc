! { dg-do compile }
! PR 85083
!
! Testcase from PR by G. Steinmetz  <gscfq@t-online.de>
!
program p
  type t
     character(3) :: c
  end type t
  type(t), allocatable :: z
  allocate (z, source=t(.true.,'abc')) ! { dg-error "Too many components" }
end
