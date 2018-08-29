! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Basic STRUCTURE test.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

! Basic structure
structure /s1/                ! type s1
  integer i1
  logical l1
  real r1
  character c1
end structure                 ! end type s1

record /s1/ r1                ! type (s1) r1
record /s1/ r1_a(3)           ! type (s1) r1_a(3)

! Basic records
r1.i1 = 13579                  ! r1%i1 = ...
r1.l1 = .true.
r1.r1 = 13.579
r1.c1 = 'F'
r1_a(2) = r1
r1_a(3).r1 = 135.79

if (r1.i1 .ne. 13579) then
  call aborts("r1.i1")
endif

if (r1.l1 .neqv. .true.) then
  call aborts("r1.l1")
endif

if (r1.r1 .ne. 13.579) then
  call aborts("r1.r1")
endif

if (r1.c1 .ne. 'F') then
  call aborts("r1.c1")
endif

if (r1_a(2).i1 .ne. 13579) then
  call aborts("r1_a(2).i1")
endif

if (r1_a(3).r1 .ne. 135.79) then
  call aborts("r1_a(3).r1")
endif

end
