! { dg-do compile }
! { dg-options "-fdec-structure -ffree-form" }
!
! Test the %FILL component extension.
!
implicit none

structure /s/
  character(2) i
  character(2) %fill
  character(2) j
end structure

structure /s2/
  character buf(6)
end structure

record /s/ x
record /s2/ y
equivalence (x, y)

x.i = "12"
x.j = "34"

if (y.buf(1) .ne. '1') then
  call abort
endif
if (y.buf(2) .ne. '2') then
  call abort
endif
if (y.buf(5) .ne. '3') then
  call abort
endif
if (y.buf(6) .ne. '4') then
  call abort
endif

end
