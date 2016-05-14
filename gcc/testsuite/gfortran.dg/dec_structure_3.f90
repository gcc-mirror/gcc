! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test nested STRUCTURE definitions.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
end subroutine

structure /s3/
  real p
  structure /s4/ recrd, recrd_a(3)
    integer i, j
  end structure
  real q
end structure

record /s3/ r3
record /s4/ r4

r3.p = 1.3579
r4.i = 0
r4.j = 1
r3.recrd = r4
r3.recrd_a(1) = r3.recrd
r3.recrd_a(2).i = 1
r3.recrd_a(2).j = 0

if (r3.p .ne. 1.3579) then
  call aborts("r3.p")
endif

if (r4.i .ne. 0) then
  call aborts("r4.i")
endif

if (r4.j .ne. 1) then
  call aborts("r4.j")
endif

if (r3.recrd.i .ne. 0 .or. r3.recrd.j .ne. 1) then
  call aborts("r3.recrd")
endif

if (r3.recrd_a(2).i .ne. 1 .or. r3.recrd_a(2).j .ne. 0) then
  call aborts("r3.recrd_a(2)")
endif

end
