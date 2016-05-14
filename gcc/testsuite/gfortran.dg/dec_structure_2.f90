! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test STRUCTUREs containin other STRUCTUREs.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
end subroutine

! Basic structure
structure /s1/
  integer i1
  logical l1
  real r1
  character c1
end structure

structure /s2/
  integer i
  record /s1/ r1
endstructure

record /s1/ r1
record /s2/ r2, r2_a(10)

! Nested and array records
r2.r1.r1 = 135.79
r2_a(3).r1.i1 = -13579

if (r2.r1.r1 .ne. 135.79) then
  call aborts("r1.r1.r1")
endif

if (r2_a(3).r1.i1 .ne. -13579) then
  call aborts("r2_a(3).r1.i1")
endif

end
