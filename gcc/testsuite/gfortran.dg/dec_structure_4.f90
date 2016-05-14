! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test anonymous STRUCTURE definitions.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
end subroutine

structure /s5/
  structure recrd, recrd_a(3)
    real x, y
  end structure
end structure

record /s5/ r5

r5.recrd.x = 1.3
r5.recrd.y = 5.7
r5.recrd_a(1) = r5.recrd
r5.recrd_a(2).x = 5.7
r5.recrd_a(2).y = 1.3

if (r5.recrd.x .ne. 1.3) then
  call aborts("r5.recrd.x")
endif

if (r5.recrd.y .ne. 5.7) then
  call aborts("r5.recrd.y")
endif

if (r5.recrd_a(1).x .ne. 1.3 .or. r5.recrd_a(1).y .ne. 5.7) then
  call aborts("r5.recrd_a(1)")
endif

if (r5.recrd_a(2).x .ne. 5.7 .or. r5.recrd_a(2).y .ne. 1.3) then
  call aborts("r5.recrd_a(2)")
endif

end
