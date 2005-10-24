! { dg-do compile }
! { dg-options "-std=f95" }
program save_2
  implicit none
  integer i
  integer foo1, foo2, foo3, foo4
  do i=1,10
     if (foo1().ne.i) then
        call abort
     end if
     if (foo2().ne.i) then
        call abort
     end if
     if (foo3().ne.i) then
        call abort
     end if
     if (foo4().ne.i) then
        call abort
     end if
  end do
end program save_2

integer function foo1
  integer j
  save
  save ! { dg-error "Blanket SAVE" }
  data j /0/
  j = j + 1
  foo1 = j
end function foo1

integer function foo2
  integer j
  save j
  save j ! { dg-error "Duplicate SAVE" }
  data j /0/
  j = j + 1
  foo2 = j
end function foo2

integer function foo3
  integer j
  save
  save j ! { dg-error "SAVE statement" }
  data j /0/
  j = j + 1
  foo3 = j
end function foo3

integer function foo4
  integer j ! { dg-error "Duplicate SAVE" }
  save j
  save
  data j /0/
  j = j + 1
  foo4 = j
end function foo4
