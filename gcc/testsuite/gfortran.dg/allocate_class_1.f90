! { dg-do compile }
!
! PR 47085: [OOP] Problem in allocate( SOURCE=) for polymorphic component
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 type :: t0
 end type
 class(t0) :: x  ! { dg-error "must be dummy, allocatable or pointer" }
 allocate(x)     ! { dg-error "is neither a data pointer nor an allocatable variable" }
 end
